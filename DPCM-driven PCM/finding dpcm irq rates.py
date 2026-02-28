def check_sum_optimized(numbers, n):
    sums1, sums2 = [], []
    numbers1 = numbers[:len(numbers) // 2]
    numbers2 = numbers[len(numbers) // 2:]

    for sums, numbers_ in ((sums1, numbers1), (sums2, numbers2)):
        for number in numbers_:
            for sum_ in sums[:]:
                sums.append(sum_ + number)

            sums.append(number)

    for sum_ in sums1:
        if n - sum_ in sums2:
            return True

    return False

def check_sum_bruteforce(numbers, n):
    # This bruteforce approach can be improved (for some cases) by
    # returning True as soon as the needed sum is found;

    sums = []

    for number in numbers:
        for sum_ in sums[:]:
            sums.append(sum_ + number)

        sums.append(number)

    return n in sums


print(check_sum_bruteforce([190,160,142,128,106,84,72,54], 11510))