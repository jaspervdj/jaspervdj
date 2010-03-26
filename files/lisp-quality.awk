#!/bin/awk -f
BEGIN {
    total = 0
    good = 0
}

{
    total += length
    gsub(/[^()]/, "")
    good += length
}

END {
    quality = good / total
    printf("Overall code quality detected as %.2f\n", quality)
    if(quality < 0.5) {
        print "The number of braces used it horrendously low. You should"
        print "seriously consider adding more braces or migrating to an"
        print "other language."
    } else {
        print "It is obvious you understand the true beauty of lisp and use"
        print "it to make your programs elegant and concise."
    }
}
