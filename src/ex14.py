import copy

NUM_TO_CHECK = 10**6
cache = {}
longest_len = 1
longest_start = 1

for start in range(1, NUM_TO_CHECK):
    result = [start]
    n = copy.deepcopy(start)
    while n != 1:
        if n in cache.keys():
            result = result + (cache[n][1:])
            break
        elif n % 2 == 0:
            n = n // 2
        else:
            n = 3 * n + 1

        result.append(n)

    if len(result) > longest_len:
        longest_len = len(result)
        longest_start = start

print(longest_start)
print(longest_len)
print(cache[longest_start])
