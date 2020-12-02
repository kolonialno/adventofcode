

def split_line(line):
    numbers, padded_letter, password = line.strip().split(" ")
    low, high = numbers.split("-")
    return low, high, padded_letter[0], password

def main():
    """
    Find the number of passwords that does not match the password policy
    """

    counter = 0 

    with open('input.txt', 'r+') as file:
        for line in file.readlines():
           low, high, letter, password = split_line(line) 
           if int(low) <= password.count(letter) <= int(high):
               print(f"Password '{password}' has between {low}-{high} occurences of '{letter}'")
               counter +=1

    print("---------------------------------------------")
    print("Passwords that match the policy:", counter)
    

if __name__ == "__main__":
    main()
