use std::collections::HashSet;
fn permutations_from_set(item: Vec<i64>, set: HashSet<i64>) -> Vec<Vec<i64>> {
    if set.is_empty() {
        return vec![item];
    }
    set.iter()
        .cloned()
        .flat_map(|d| {
            let mut next_item = item.clone();
            next_item[set.len() - 1] = d;
            let mut next_set = set.clone();
            next_set.remove(&d);
            permutations_from_set(next_item, next_set)
        })
        .collect()
}
pub fn permutations_in_memory(items: Vec<i64>) -> Vec<Vec<i64>> {
    permutations_from_set(items.clone(), items.iter().cloned().collect())
}

#[cfg(test)]
mod tests {
    #[test]
    fn permutations_1() {
        let permutations = super::permutations_in_memory((0..5).collect());
        assert_eq!(permutations.len(), 120);
    }
}
