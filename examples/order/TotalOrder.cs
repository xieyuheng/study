module order

class TotalOrder extends PartialOrder {
  connex(a: E, b: E): Either[a <= b, b <= a]
}

// quick sort
// bubble sort
// merge sort
