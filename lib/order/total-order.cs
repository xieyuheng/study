module order

class total_order_t extends partial_order_t {
  connex(a: E, b: E): either_t(a <= b, b <= a)
}

// quick sort
// bubble sort
// merge sort
