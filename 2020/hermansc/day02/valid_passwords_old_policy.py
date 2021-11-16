valid_count = 0
with open("corrupted_password_database.txt", "r") as fh:
    for line in fh:

        # Parse
        policy_counts, policy_letter, password = line.split(" ")
        policy_letter = policy_letter[0]
        password = password.strip()
        policy_min_count, policy_max_count = policy_counts.split("-")
        policy_min_count, policy_max_count = int(policy_min_count), int(policy_max_count)

        # Evaluate
        num_in_password = password.count(policy_letter)
        if policy_min_count <= num_in_password <= policy_max_count:
            valid_count += 1

print(valid_count)
