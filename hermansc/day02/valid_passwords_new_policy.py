valid_count = 0
with open("corrupted_password_database.txt", "r") as fh:
    for line in fh:

        # Parse
        policy_counts, policy_letter, password = line.split(" ")
        policy_letter = policy_letter[0]
        password = password.strip()
        policy_p0_position, policy_p1_position = policy_counts.split("-")
        policy_p0_position, policy_p1_position = int(policy_p0_position) - 1, int(policy_p1_position) - 1

        # Evaluate

        if len(
            [b for b in [password[policy_p0_position] == policy_letter, password[policy_p1_position] == policy_letter] if b]
        ) == 1:
            valid_count += 1

print(valid_count)
