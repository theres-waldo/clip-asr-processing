#![allow(non_snake_case)]
#![allow(dead_code)]

use std::rc::Rc;
use std::cell::RefCell;
use std::collections::HashSet;

type ScrollId = u32;

#[derive(Debug, PartialEq)]
pub struct LayerTree<Annotation> {
    annotation: Annotation,
    children: Vec<LayerTree<Annotation>>
}
mod input {
    use super::*;

    pub type ASRNodeRef = Rc<RefCell<ASRNode>>;

    #[derive(Debug)]
    pub struct ASRNode {
        pub scrollId: ScrollId,
        pub children: Vec<ASRNodeRef>,
        pub parent: Option<ASRNodeRef>
    }

    #[derive(Debug, PartialEq)]
    pub struct InputAnnotation {
        pub scrollsWith: ScrollId,
        pub clips: Vec<ScrollId>
    }

    pub type InputLayerTree = LayerTree<InputAnnotation>;
}

mod output {
    use super::*;

    #[derive(Debug, PartialEq)]
    pub struct Metrics {
        pub scrollId: ScrollId,
        pub hasScrollClip: bool,
    }

    #[derive(Debug, PartialEq)]
    pub struct OutputAnnotation {
        pub metrics: Vec<Metrics>,
        pub hasScrolledClip: bool,
        pub fixedTo: Option<ScrollId>
    }

    pub type OutputLayerTree = LayerTree<OutputAnnotation>;
}

use self::input::*;
use self::output::*;

fn findASRNode(scrollId: ScrollId, asrTree: ASRNodeRef) -> Option<ASRNodeRef> {
    let node = asrTree.borrow();
    if node.scrollId == scrollId {
        return Some(asrTree.clone());
    }
    for child in &node.children {
        if let Some(result) = findASRNode(scrollId, child.clone()) {
            return Some(result);
        }
    }
    return None;
}

// Compute the chain of ASRs from |from| to the root.
fn scrollChain(from: Option<ScrollId>, asrTree: ASRNodeRef) -> Vec<ScrollId> {
    if from.is_none() {
        return vec![];
    }
    let from = from.unwrap();
    let mut result = vec![];
    let mut node = findASRNode(from, asrTree);
    while node.is_some() {
        result.push(node.as_ref().unwrap().borrow().scrollId);
        let parent = node.as_ref().unwrap().borrow().parent.clone();
        node = parent;
    }
    result
}

// Given two chains |a| and |b|, each from an ASR to the root (and thus sharing a common
// suffix, even it's just the root), return the prefix of |a| that excludes that common suffix.
// Conceptually, this computes "the additional things that scroll |a| but do not scroll |b|".
fn prefixChain(mut a: Vec<ScrollId>, mut b: Vec<ScrollId>) -> Vec<ScrollId> {
    while !a.is_empty() && !b.is_empty() && a.last().unwrap() == b.last().unwrap() {
        a.pop();
        b.pop();
    }
    a
}

// Get the parent of the ASR |id| in the ASR tree.
fn scrollParent(id: ScrollId, asrTree: ASRNodeRef) -> Option<ScrollId> {
    if let Some(node) = findASRNode(id, asrTree) {
        if let Some(parent) = &node.borrow().parent {
            return Some(parent.borrow().scrollId);
        }
    }
    None
}

// Get the child of the ASR |parent| in the scroll chain |chain|.
fn childInChain(parent: ScrollId, chain: &Vec<ScrollId>, asrTree: ASRNodeRef) -> Option<ScrollId> {
    for id in chain {
        if let Some(parentId) = scrollParent(*id, asrTree.clone()) {
            if parentId == parent {
                return Some(*id)
            }
        }
    }
    None
}

// Transform a layer subtree in the input layer tree to the corresponding layer subtree
// in the output layer tree.
// |parentScrollsWith| is the ASR of the parent layer of the subtree's root layer, or None
// if the subtree is the entire tree.
fn transformNode(input: &InputLayerTree, asrTree: ASRNodeRef, parentScrollsWith: Option<ScrollId>) -> OutputLayerTree {
    // The ASR for this layer.
    let scrollsWith = input.annotation.scrollsWith;

    // The chain of ASRs from this layer's to the root.
    let chain = scrollChain(Some(scrollsWith), asrTree.clone());

    // The chain of ASRs from the parent layer's to the root.
    let parentChain = scrollChain(parentScrollsWith, asrTree.clone());

    // The ASRs that scroll this layer but not the parent layer.
    // Each of these will generate a metrics on this layer.
    let mut scrollIds = prefixChain(chain, parentChain.clone());

    // If this layer will have a scrolled clip, the ASR the clip scrolls with.
    let mut scrolledClip : Option<ScrollId> = None;

    // Stores which of the metrics on this layer have a scroll clip.
    let mut clips: HashSet<ScrollId> = HashSet::new();

    // Handle the clips in the input representation.
    for clip in &input.annotation.clips {
        // The common case is that the clip scrolls with an ASR in our scroll chain.
        // Find its child ASR in our scroll chain, and mark that metrics as having a scroll clip.
        if let Some(id) = childInChain(*clip, &scrollIds, asrTree.clone()) {
            clips.insert(id);
        } else {
            // Otherwise, we have a scrolled clip. Record its scroll id for later use.
            // We only allow one scrolled clip per layer.
            if scrolledClip.is_some() {
                panic!("Unexpected: multiple scrolled clips on a layer");
            }
            scrolledClip = Some(*clip);
        }
    }

    // Now compute the layer's fixed annotation, if any.
    // There are two scenarios that create a fixed annotation.
    let mut fixedTo = None;

    // The first scenario is when we have a scrolled clip.
    // The scrolled clip requires adding a metrics for the ASR that scrolls the clip
    // (so that the clip will scroll), while adding a fixed annotation to the layer
    // for the same ASR (so that the layer contents will not).
    if let Some(scrolledClip) = scrolledClip {
        if scrollIds.contains(&scrolledClip) {
            panic!("Unexpected: already have metrics for scrolled clip's scroll id");
        }
        scrollIds.insert(0, scrolledClip);
        fixedTo = Some(scrolledClip);
    }

    // The second (and more common) scenario is when |scrollIds| is empty (so there
    // are not ASRs that scroll this layer but not the parent), but the ASR that
    // scrolls this layer is not the one that scrolls the parent layer (in which case,
    // it must be some ancestor ASR).
    // In this case, find the child of the ASR that scrolls this layer in our parent's
    // scroll chain, and mark us as being fixed with respect to that.
    if scrollIds.is_empty() && parentScrollsWith.is_some() && parentScrollsWith.unwrap() != scrollsWith {
        if let Some(id) = childInChain(scrollsWith, &parentChain, asrTree.clone()) {
            fixedTo = Some(id);
        } else {
            panic!("Unexpected: can't find what we're fixed w.r.t.")
        }
    }

    // Finally, compute our metrics from |scrollIds| and |clips|.
    let metrics = scrollIds.iter()
        .map(|id| Metrics { scrollId: *id, hasScrollClip: clips.contains(id) } )
        .filter(|m| m.scrollId != 0).collect();

    // Put the pieces together.
    OutputLayerTree {
        annotation: OutputAnnotation {
            metrics,
            hasScrolledClip: scrolledClip.is_some(),
            fixedTo,
        },
        // When descending into our child layers, the ASR that scrolls this layer becomes
        // the |parentScrollsWith| for child layers.
        children: input.children.iter().map(
            |node| transformNode(node, asrTree.clone(), Some(scrollsWith))).collect()
    }
}

// Transform an input layer tree + ASR tree, to an output layer tree.
fn transformTree(input: InputLayerTree, asrTree: ASRNodeRef) -> OutputLayerTree {
    // Dispatch to a recursive helper method, which tracks one additional piece of state:
    // the ASR of the parent layer of the layer being processed.
    // Initially, we're processing the root layer, so there is no parent layer, so we pass None.
    transformNode(&input, asrTree, None)
}

#[cfg(test)]
mod tests {
    use crate::*;

    // Build the ASR tree used by all the tests.
    // It looks like this:
    //      0
    //    /   \
    //    1   2
    //    |
    //    3
    fn buildASRTree() -> ASRNodeRef {
        let c = Rc::new(RefCell::new(ASRNode {
            scrollId: 3,
            children: vec![],
            parent: None
        }));
        let a = Rc::new(RefCell::new(ASRNode {
            scrollId: 1,
            children: vec![c.clone()],
            parent: None
        }));
        let b = Rc::new(RefCell::new(ASRNode {
            scrollId: 2,
            children: vec![],
            parent: None
        }));
        let r = Rc::new(RefCell::new(ASRNode {
            scrollId: 0,
            children: vec![a.clone(), b.clone()],
            parent: None
        }));
        a.borrow_mut().parent = Some(r.clone());
        b.borrow_mut().parent = Some(r.clone());
        c.borrow_mut().parent = Some(a.clone());
        r
    }

    // Helper functions for creating the input and output tree structures.
    fn mkInputNode(scrollId: ScrollId, clips: Vec<ScrollId>, children: Vec<InputLayerTree>) -> InputLayerTree {
        InputLayerTree {
            annotation: InputAnnotation {
                scrollsWith: scrollId,
                clips
            },
            children
        }
    }
    fn mkMetrics(scrollId: ScrollId, hasScrollClip: bool) -> Metrics {
        Metrics {
            scrollId,
            hasScrollClip,
        }
    }
    fn mkOutputNode(metrics: Vec<Metrics>, hasScrolledClip: bool, fixedTo: Option<ScrollId>, children: Vec<OutputLayerTree>) -> OutputLayerTree {
        OutputLayerTree {
            annotation: OutputAnnotation {
                metrics,
                hasScrolledClip,
                fixedTo
            },
            children
        }
    }

    #[test]
    fn tree1() {
        let input =
            mkInputNode(0, vec![], vec![
                mkInputNode(1, vec![0], vec![
                    mkInputNode(3, vec![1], vec![])])]);
        let output = transformTree(input, buildASRTree());
        let expected =
            mkOutputNode(vec![], false, None, vec![
                mkOutputNode(vec![mkMetrics(1, true)], false, None, vec![
                    mkOutputNode(vec![mkMetrics(3, true)], false, None, vec![])])]);
        assert_eq!(expected, output);
    }

    #[test]
    fn tree2() {
        let input =
            mkInputNode(0, vec![], vec![
                mkInputNode(3, vec![0, 1], vec![])]);
        let output = transformTree(input, buildASRTree());
        let expected =
            mkOutputNode(vec![], false, None, vec![
                mkOutputNode(vec![mkMetrics(3, true), mkMetrics(1, true)], false, None, vec![])]);
        assert_eq!(expected, output);
    }

    #[test]
    fn tree3() {
        let input =
            mkInputNode(0, vec![], vec![
                mkInputNode(1, vec![0], vec![
                    mkInputNode(0, vec![], vec![])])]);
        let output = transformTree(input, buildASRTree());
        let expected =
            mkOutputNode(vec![], false, None, vec![
                mkOutputNode(vec![mkMetrics(1, true)], false, None, vec![
                    mkOutputNode(vec![], false, Some(1), vec![])])]);
        assert_eq!(expected, output);
    }

    #[test]
    fn tree4() {
        let input =
            mkInputNode(0, vec![], vec![
                mkInputNode(1, vec![0], vec![]),
                mkInputNode(0, vec![1], vec![
                    mkInputNode(2, vec![0], vec![])])]);
        let output = transformTree(input, buildASRTree());
        let expected =
            mkOutputNode(vec![], false, None, vec![
                mkOutputNode(vec![mkMetrics(1, true)], false, None, vec![]),
                mkOutputNode(vec![mkMetrics(1, false)], true, Some(1), vec![
                    mkOutputNode(vec![mkMetrics(2, true)], false, None, vec![])])]);
        assert_eq!(expected, output);
    }

    #[test]
    fn tree5() {
        let input =
            mkInputNode(1, vec![0], vec![
                mkInputNode(1, vec![], vec![]),
                mkInputNode(0, vec![], vec![
                    mkInputNode(2, vec![0], vec![])])]);
        let output = transformTree(input, buildASRTree());
        let expected =
            mkOutputNode(vec![mkMetrics(1, true)], false, None, vec![
                mkOutputNode(vec![], false, None, vec![]),
                mkOutputNode(vec![], false, Some(1), vec![
                    mkOutputNode(vec![mkMetrics(2, true)], false, None, vec![])])]);
        assert_eq!(expected, output);
    }
}
