
(use test archetype)

(test-begin)

(test '("this is " (bold "not") " TeX!")
    (archetype-parse "this is \\bold{not} TeX!"))

(test "blah blah blah"
    (archetype->html (archetype-parse "\\define{\\x blah}\\x \\x \\x")))

(test "this is <b>*bold*</b> text"
    (archetype->html (archetype-parse "\\define{\\bold \\lambda{\\{x} \\b{\\x}}}this is \\bold{*bold*} text")))

(test "this is <b>*bold*</b> text"
    (archetype->html (archetype-parse "\\define{\\{bold x} \\b{\\x}}this is \\bold{*bold*} text")))

(test "this is <b>*bold*</b> text"
    (archetype->html (archetype-parse "\\define{\\bold{x} \\b{\\x}}this is \\bold{*bold*} text")))

(test-end)

