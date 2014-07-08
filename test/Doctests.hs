{-# LANGUAGE CPP #-}

import Test.DocTest
main = doctest ["-isrc",
#ifdef LIBEDITLINE
        "-DLIBEDITLINE",
#endif
        "src/Main.hs"]

