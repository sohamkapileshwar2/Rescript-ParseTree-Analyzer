module StringArray = {
  type t = array<string>

  let empty = []
}

module Empty = {}

module Empty = {
  // TODO: convince management to implement this
}

module Empty = {/* test */}

module EmptyModule = {
  /* TODO: management on vacation */
}

module type T = {}

let g = {
  module M: T = {}
  0
}