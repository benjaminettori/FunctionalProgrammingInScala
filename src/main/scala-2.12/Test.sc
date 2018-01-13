val myString = "\bHello\b"
val regex = myString.r

regex.pattern.matcher("Ben").matches()
regex.pattern.matcher("Hello Goodbye").matches()
regex.regex
