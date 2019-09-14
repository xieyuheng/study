module order

class total_order_t extends partial_order_t {
  connex(a: E, b: E): either_t(pre_t(a, b), pre_t(b, a))
}

// quick sort
// bubble sort
// merge sort
