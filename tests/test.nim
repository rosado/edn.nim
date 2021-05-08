# To run these tests, simply execute `nimble test`.

import unittest
import edn, options, tables, strutils

# need to re-structure this at some point
test "everything":
  var node: EdnNode

  node = read(" nil")
  check node.kind == EdnNil

  node = read("10")
  check node.kind == EdnInt
  check node.num == 10

  node = read("10e10")
  check node.kind == EdnFloat
  check node.fnum == 10e10

  node = read("+5.0E5")
  check node.kind == EdnFloat
  check node.fnum == +5.0E5

  node = read("true")
  check node.kind == EdnBool
  check node.boolVal == true

  node = read("1 2 3")
  check node.kind == EdnInt
  check node.num == 1

  node = read("(1 2 3)")
  check node.kind == EdnList
  check node.list.len == 3

  node = read("""(
                  ;; comment in a list
                   )""")
  check node.kind == EdnList
  check node.comments.len == 0

  block:
    # comment related tests
    
    var opts: ParseOptions
    opts.eof_is_error = true
    opts.suppress_read = false
    opts.conditional_exprs = asError
    opts.comments_handling = keepComments
    node = read(""";; this is a coment
    ()
    """, opts)
    check node.kind == EdnCommentLine
    check node.comments.len == 0

    node = read("""(
    ;; this is a coment
    ())
    """, opts)
    check node.kind == EdnList
    check node.list[0].comments.len > 0
    
    node = read(""";; this is a comment
    (1 2
    ;; last elem
    3)""", opts)
    check node.kind == EdnCommentLine

    # the comment should be returned on subsequent read().
    # not very clean, but does not require a look-ahead read()
    node = read("""()
;; comment after a list""", opts)
    check node.kind == EdnList
    check node.comments.len == 0

    node = read("""(
                  ;; comment in a list
                   )""", opts)
    check node.kind == EdnList
    check node.comments.len == 1
    check node.comments[0].placement == Inside
    
    node = read(""";; this is a comment
    (1 2
    ;; last elem
    3)""", opts)
    check node.kind == EdnCommentLine
    check node.comments.len == 0

    node = read("""{:x 1
                    ;;comment 
                    :y 2} """, opts)
    check node.kind == EdnMap

    node = read("""{:view s/Keyword
                    ;;comment 
                    (s/optional-key :label) s/Str
                    (foo 1) 2} """, opts)
    check node.kind == EdnMap


  node = read("""{:view s/Keyword
                  ;;comment 
                  (s/optional-key :label) s/Str
                  (foo 1) 2} """)
  check node.kind == EdnMap

  node = read(""";; this is a comment
  (1 2
  ;; last elem
  3)""")
  check node.kind == EdnList
  check node.comments.len == 0
  check node.list[2].comments.len == 0

  node = read("1")
  check node.kind == EdnInt
  check node.num == 1

  node = read("-1")
  check node.kind == EdnInt
  check node.num == -1

  node = read("1M")
  check node.kind == EdnInt     #TODO: for now...

  node = read("()")
  check node.kind == EdnList
  check node.list.len == 0

  node = read("(())")
  check node.kind == EdnList
  check node.list.len == 1
  check node.list[0].kind == EdnList
  check node.list[0].list.len == 0

  node = read("nil")
  check node.kind == EdnNil

  node = read("symbol-👋") #emoji
  check node.kind == EdnSymbol
  check node.symbol.name == "symbol-👋"

  node = read(":foo")
  check node.kind == EdnKeyword
  check node.keyword.name == "foo"
  check node.namespacing == NoNamespace
  check $node == ":foo"

  node = read("::foobar")
  check node.kind == EdnKeyword
  check node.keyword.name == "foobar"
  check node.keyword.ns == ""
  check node.namespacing == LocalNamespace
  check $node == "::foobar"

  node = read("+foo+")
  check node.kind == EdnSymbol
  check node.symbol.name == "+foo+"

  node = read("moo/bar")
  check node.kind == EdnSymbol
  check node.symbol == ("moo", "bar")

  node = read("'foo") # -> (quote foo)
  check node.kind == EdnList
  check node.list[0] == new_edn_symbol("", "quote")

  node = read("{}")
  check node.kind == EdnMap
  check node.map.len == 0

  node = read("{:A 1 :B 2}")
  check node.kind == EdnMap
  check node.map.len == 2

  node = read("{:A 1, :B 2}")
  check node.kind == EdnMap
  check node.map.len == 2

  node = read("{:x 1M :y 2}")
  check node.kind == EdnMap
  check node.map.len == 2

  node = read("{:order_date #clj-time/date-time \"2019-12-01T00:00:00.000Z\", :quantity 125.3M, 1 1}")
  check node.kind == EdnMap
  check node.map.len == 3
  
  try:
    node = read("moo/bar/baz")
    raise new_exception(Exception, "FAILURE")
  except ParseError:
    discard

  node = read("[1 2 , 3,4]")
  check node.kind == EdnVector
  check node.vec.len == 4

  node = read("^{:k 1} {}")
  check node.kind == EdnMap
  check node.map.count == 0
  # TODO: define 'len' for HMap
  check node.map_meta.count == 1
  check node.map_meta[new_edn_keyword("", "k")].get() == new_edn_int(1)

  let hh = new_hmap()
  hh[new_edn_keyword("", "foo")] = new_edn_bool(true)
  check hh[new_edn_keyword("", "foo")].get() == new_edn_bool(true)

  node = read("^ :foo []")
  check node.kind == EdnVector
  check node.vec.len == 0
  check node.vec_meta.count == 1
  check node.vec_meta[new_edn_keyword("", "foo")].get() == new_edn_bool(true)

  node = read("^foo (1 2 3)")
  check node.kind == EdnList
  check node.list.len == 3
  check node.list_meta.count == 1
  check node.list_meta[KeyTag].get() == new_edn_symbol("", "foo")

  node = read("^{:x 1} #{1}")
  check node.kind == EdnSet
  check node.set_meta.count == 1

  node = read("^\"foo\" Symbol")
  check node.kind == EdnSymbol
  check node.symbol == new_edn_symbol("", "Symbol").symbol
  check node.symbol_meta[KeyTag].get().kind == EdnString
  check node.symbol_meta[KeyTag].get().str == "foo"

  node = read("\"foo\"")
  check node.kind == EdnString
  check node.str == "foo"
  check node.str.len == 3

  node = read("#_ [foo bar]")
  check node == nil

  node = read("#{foo whateve 1}")
  check node.kind == EdnSet
  check node.set_elems.count == 3

  node = read("#{}")
  check node.kind == EdnSet
  check node.set_elems.count == 0

  node = read("#:foo {:x 1}")
  check node.kind == EdnMap
  check node.map.count == 1
  check node.map[new_edn_keyword("foo", "x")].get == new_edn_int(1)

  node = read("#:foo {:_/x 1}")
  check node.kind == EdnMap
  check node.map.count == 1
  check node.map[new_edn_keyword("", "x")].get == new_edn_int(1)

  node = read("1/2")
  check node.kind == EdnRatio
  check node.rnum == (BiggestInt(1), BiggestInt(2))

  node = read("{:ratio -1/2}")
  check node.kind == EdnMap
  check node.map[new_edn_keyword("", "ratio")].get == new_edn_ratio(-1, 2)

  node = read("#foo.bar -1")
  check node.kind == EdnTaggedValue
  check node.value.kind == EdnInt
  check node.value == new_edn_int(-1)

  node = read("#foo.bar [1 2 \"balls\"]")
  check node.kind == EdnTaggedValue
  check node.value.kind == EdnVector

  node = read("#(or % disabled)")
  check node.kind == EdnList

  # let's set up conditional forms reading
  var opts: ParseOptions
  opts.conditional_exprs = asTagged
  init_edn_readers(opts)

  # conditional compilation exprs
  node = read("#+clj #{foo}")
  check node.tag == ("", "+clj")
  check node.kind == EdnTaggedValue
  check node.value.kind == EdnSet

  opts.conditional_exprs = cljSource
  init_edn_readers(opts)
  node = read("#+clj #{foo}")
  check node.kind == EdnSet
  node = read("#+cljs {}")
  check node == nil

  node = read("[1 2 #+cljs 3 4]")
  check node.kind == EdnVector
  check node.vec.len == 3

  var opts1: ParseOptions
  opts1.eof_is_error = true
  opts1.suppress_read = false
  opts1.conditional_exprs = cljSource

  node = read("#?(:clj :x)", opts1)
  check node.kind == EdnKeyword

  node = read("#?(:cljs :x)", opts1)
  check node == nil

  try:
    node = read("#?(:cljs :x :clj)", opts1)
    check false
  except ParseError:
    discard

  node = read("[1 2 #?(:clj 3)]", opts1)
  check node.kind == EdnVector
  check node.vec.len == 3

  opts1.conditional_exprs = cljsSource
  node = read("[1 2 #?(:clj 3)]", opts1)
  check node.kind == EdnVector
  check node.vec.len == 2

  opts1.conditional_exprs = ignoreConditionals
  node = read("#?@(:clj [:generator-fn tlsubs/subscriber-timeline-record-generator])", opts1)
  check node == nil

  opts1.conditional_exprs = cljSource
  # this should splice the tuple into key & value of the map,
  #so result should be a map with one entry in it.
  node = read("{#?@(:clj [:generator-fn tlsubs/subscriber-timeline-record-generator])}", opts1)
  check node.kind == EdnMap
  check node.map[new_edn_keyword("", "generator-fn")].get().kind == EdnSymbol

  try:
    node = read("{:ratio 1/-2}")
    check node.kind == EdnMap
  except ParseError:
    discard

  try:
    node = read(";; foo bar")
    check false
  except ParseError:
    discard

  node = read("\"\uffff\"")
  check node.kind == EdnString

  node = read("\\uffff")
  check node.kind == EdnCharacter

  node = read("()") # for the following to work
  var n1: EdnNode = EdnNode(kind: EdnNil)
  var n2: EdnNode = EdnNode(kind: EdnNil)
  var n3: EdnNode = EdnNode(kind: EdnBool, boolVal: false)
  var n4: EdnNode = EdnNode(kind: EdnBool, boolVal: false)
  #echo "===? ", n1 == n2
  var t = new_table[EdnNode,int]()
  t[n3] = 3
  #echo "COUNT OF ELEMS ", t.len, " ", n1.hash, " ", n2.hash, " ", n3.hash
  t[n4] = 4
  #echo "COUNT OF ELEMS ", t.len, " ", n1.hash, " ", n2.hash, " ", n3.hash
  t[n4] = 5
  #echo "COUNT OF ELEMS ", t.len, " ", n1.hash, " ", n2.hash, " ", n3.hash

  var mm1 = new_hmap()
  mm1[n2] = n2
  mm1[node] = node

  mm1 = new_hmap(0)
  mm1[node] = node
  check mm1.count == 1
  mm1[n1] = n1
  check mm1.count == 2
  mm1[n1] = n2
  check mm1.count == 2
  for i in 1..10:
    mm1[new_edn_int(i.int_to_str())] = new_edn_int(i.int_to_str())
  check mm1.count == 12
  check mm1[n1].get() == n2

  node = read("#\".*\"")
  check node.kind == EdnRegex

  node = read("#'some-ns/symbol")
  check node.kind == EdnVarQuote


