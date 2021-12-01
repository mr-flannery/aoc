(->> [199 200 208 210 200 207 240 269 260 263]
     (partition 2 1)
     (filter (fn [[first second]] (> second first)))
     count)