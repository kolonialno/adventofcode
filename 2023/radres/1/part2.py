numbers_as_words = ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
word_to_number_string = {
    "zero": "0",
    "one": "1",
    "two": "2",
    "three": "3",
    "four": "4",
    "five": "5",
    "six": "6",
    "seven": "7",
    "eight": "8",
    "nine": "9"
}
sum = 0
for string in out:
    first = 99999
    last = -1
    first_digit = ""
    last_digit = ""
    for word in numbers_as_words:
        if string.find(word) < first and string.find(word) > -1:
            first_digit = word_to_number_string[word]
            first = string.find(word)
        if string.rfind(word) > last and string.rfind(word) > -1:
            last_digit = word_to_number_string[word]
            last = string.rfind(word)
        if string.find(word_to_number_string[word]) < first and string.find(word_to_number_string[word]) > -1:
            first_digit = word_to_number_string[word]
            first = string.find(word_to_number_string[word])
        
        if string.rfind(word_to_number_string[word]) > last and string.rfind(word_to_number_string[word]) > -1:
            last_digit = word_to_number_string[word]
            last = string.rfind(word_to_number_string[word])
    print(string)
    print("first: " + first_digit, "last: " + last_digit)
    if not first_digit or not last_digit:
        continue
    sum += int(first_digit + last_digit)                
print(sum)
