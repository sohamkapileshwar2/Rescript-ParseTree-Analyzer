module rec A: Map = { let m = 2}
and B: Set =  {let s = 1}

@onFirstBinding
module rec A: Map = { let m = 2}
@onSecondBinding
and B: Set =  {let s = 1}

@onFirstBindingOfNext
module rec A: Map = { let m = 2}
@onSecondBindingOfNext
and B: Set =  {let s = 1}
