library("testthat")
library("qualtrics")

if (Sys.getenv("AWS_ACCESS_KEY_ID") != "") {
    test_check("qualtrics", filter = "authenticated")
}

test_check("qualtrics", filter = "public")
