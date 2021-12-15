```
start -- A -- end
       /   \
      b --- c
```

start (A) [start] (start)
A (b c end) [start] (start-A)
b (A c c end) [start b] (start-A-b)
A (c end c c end) [start b] (start-A-b-A)
c (A end c c end) [start b c] (start-A-b-A-c)
A (end c c end) [start b c] (stat-A-b-A-c-A)
end (c c end) [start b c] (start-A-b-A-c-A-end) X
A (c c end) [start b c] (start-A-b-A-c-A-end)
c (c end) [start b c] ()