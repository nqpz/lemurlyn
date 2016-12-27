t = lambda n: 20 * n + 10
u = lambda n: 13 * n + 15
v = lambda n: 81 * 8 * n + 20
w = lambda n: 19 * 2 * n + 25

def find(top, f):
    prev = None
    for i in range(top + 1):
        cur = f(i)
        if cur > top:
            return prev
        prev = cur

res = []
for i in range(10, 10000):
    it = find(i, t)
    iu = find(i, u)
    iv = find(i, v)
    iw = find(i, w)
    if it is None or iu is None or iv is None or iw is None:
        continue
    diff = (abs(t(it) - u(iu)) +
            abs(t(it) - v(iv)) +
            abs(t(it) - w(iw)) +
            abs(u(iu) - v(iv)) +
            abs(u(iu) - w(iw)) +
            abs(v(iv) - w(iw)))
    score = diff # * i
    res.append((score, (it, iu, iv, iw)))

res.sort(key=lambda t: t[0])
print(res)
