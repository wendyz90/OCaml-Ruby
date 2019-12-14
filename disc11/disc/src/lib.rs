// Discussion 11 exercises, due 4/20/2018

// HOW TO SUBMIT:
// Upload lib.rs via web submission.

// IF YOU ARE USING GRACE: You can download lib.rs on
// https://dav.terpconnect.umd.edu

use std::collections::HashMap;
use std::ops::Add;

// Returns all substrings (as slices), excluding the empty string.
// substrings("abc") -> ["a", "ab", "abc", "b", "bc", "c"]

pub fn substrings(s: &str) -> Vec<&str> {
  let mut v = Vec::<&str>::new();
  for i in 1..(s.len()+1) {
      for j in 0..i {
          v.push(&s[j..i]);
      }
  };
  v
}

// Sorts the array w/ the quicksort algorithm. partition is given.

fn partition(arr: &mut [i32]) -> usize {
    let p = arr[arr.len() - 1];
    let mut i: usize = 0;
    for j in i..(arr.len() - 1) {
        if arr[j] < p {
            arr.swap(i, j);
            i += 1;
        }
    }
    let len = arr.len() - 1;
    arr.swap(i, len);
    i
}

pub fn quicksort(arr: &mut [i32]) {
    if arr.len()>1{
        let p = partition(arr);
        quicksort(&mut arr[..p]);
        quicksort(&mut arr[(p+1)..]);
    }
}

// Given 2 word counters (hashmaps which map strings to integers) a and b, get the total count of &str k across both counters.
// if a is { "a": 4, "b": 2 } and b is { "b": 2 }...
// get_from_both(&a, &b, "a") -> 4
// get_from_both(&a, &b, "b") -> 4
// get_from_both(&a, &b, "c") -> 0

pub fn get_from_both(a: &HashMap<&str, i32>, b: &HashMap<&str, i32>, k: &str) -> i32 {
  let x = match a.get(k) {
      Some(i) => *i,
      None => 0,
  };
  let y = match b.get(k) {
      Some(i) => *i,
      None => 0,
  };
  x+y
}

// Overload operator + for Vec2.
// Vec2 { x: 1., y: 2. } + Vec2 { x: 3., y: 4. } -> Vec2 { x: 4., y: 6. }

#[derive (Debug, PartialEq)]
struct Vec2 {
    x: f64,
    y: f64,
}

impl Add for Vec2 {
    type Output = Vec2;

    fn add(self, other: Vec2) -> Vec2 {
        Vec2 {
            x: self.x +other.x,
            y: self.y + other.y,
        }
    }
}

#[cfg(test)]
mod public;
