# Test used by examples/list-fns.rs
rec {
  # Usage: add x y
  # Adds the integers x and y together and returns the result
  add =
    # First integer
    x:
    # Second integer
    y:
    x + y;
  # Usage: sum nums
  # Returns the sum of the integer array nums
  sum = nums: builtins.foldl' add 0 nums;
}
