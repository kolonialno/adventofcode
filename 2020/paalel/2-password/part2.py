from part1 import split_line

def main():
    """
    Find the number of passwords that does not match the password policy
    """

    counter = 0 

    with open('input.txt', 'r+') as file:
        for line in file.readlines():
           low, high, letter, password = split_line(line) 
           # Exclusive or
           if (password[int(low)-1] == letter) is not (password[int(high)-1] == letter):
               print(f"Password '{password}' matches policy.")
               counter +=1

    print("---------------------------------------------")
    print("Passwords that match the policy:", counter)
    

if __name__ == "__main__":
    main()
