import FungeBase
import RunFunge
import RandomFunge
import Control.Monad
import Data.Maybe
import Data.List

import Data.IORef
import System.Posix.Signals

run_1_core dim n p = doRandomFunge (runFor n) [dim] [0] [1] (defaultConfig { charSet = chars1D, trace = noTrace, acceptUserInput = False, activeExtensions = "" {-, printer = p -} })

run_2_core dim n p = doRandomFunge (runFor n) [dim, dim] [0, 0] [1, 0] (defaultConfig { charSet = chars2D, trace = noTrace, acceptUserInput = False, activeExtensions = "", printer = p })

run_3_core dim n p = doRandomFunge (runFor n) [dim, dim, dim] [0, 0, 0] [1, 0, 0] (defaultConfig { charSet = chars3D, trace = noTrace, acceptUserInput = False, activeExtensions = "", printer = p })

go core dim n runs = do
  currentProgram <- newIORef ""
  installHandler sigINT (CatchOnce (do { p <- readIORef currentProgram; putStrLn ("killed while executing " ++ p); raiseSignal sigINT})) Nothing  
  result <- sequence . map (\(a,b) -> do {putStrLn (show b) ; a}) $ zip (replicate runs $ core dim n (\a b -> writeIORef currentProgram (funge2DToString a b))) [0..(runs - 1)]
  let terminators = sort . map snd . catMaybes . map fst $ result
  let counts = map (\a -> (head a, length a)) . group $ terminators
  putStrLn "----"
  putStrLn ""
  sequence_ $ map (\(val, num) -> putStrLn (show val ++ ": " ++ show (fromIntegral num / fromIntegral runs * 100.0))) counts
  putStrLn ("~: " ++ show (fromIntegral (runs - length terminators) / fromIntegral runs * 100.0))


{-- 

main = go run_1_core 10 100 100000

1: 7.475
2: 2.675
3: 2.456
4: 1.822
5: 1.560
6: 1.281
7: 1.142
8: 0.889
9: 0.604
10: 0.367
11: 0.302
12: 0.206
13: 0.271
14: 0.215
15: 0.178
16: 0.145
17: 0.141
18: 0.103
19: 0.080
20: 0.035
21: 0.036
22: 0.017
23: 0.022
24: 0.019
25: 0.031
26: 0.028
27: 0.030
28: 0.030
29: 0.034
30: 0.030
31: 0.025
32: 0.025
33: 0.026
34: 0.018
35: 0.019
36: 0.014
37: 0.009
38: 0.004
39: 0.001
40: 0.009
41: 0.005
42: 0.005
43: 0.004
44: 0.004
45: 0.001
46: 0.003
47: 0.003
48: 0.003
49: 0.001
50: 0.001
51: 0.001
52: 0.002
53: 0.002
54: 0.001
55: 0.003
59: 0.001
60: 0.001
65: 0.001
66: 0.001
67: 0.002
69: 0.001
70: 0.001
71: 0.003
75: 0.001
79: 0.002
80: 0.001
83: 0.001
85: 0.001
87: 0.001
91: 0.001
93: 0.001
94: 0.001
98: 0.001
~: 77.565

--}

{--

main = go run_1_core 100 1000 100000


b1!%!7a,$<:4+x8[<f<ug+ 8d4drf#c675$#a30b2[gp ,c.q2ar*p21dk5ek4:d-6\"asg0gaj$k4\\j}85k6`k6%u%/}e-{{$$'c
 - woah. abs -2147483648 == -2147483648

?7 dj3j j\"ws-'\\,.k<@\"+]k2/6xb9!0[7\\-089q-s@+7q0[!>,]!u$,<;!1e3>686xx]%\"6 ,:1@eaj%*k@+wc8>8;xb14d!?$g
 - another missing cellToData.

1: 7.557
2: 2.635
3: 2.503
4: 1.935
5: 1.729
6: 1.371
7: 1.266
8: 1.004
9: 0.933
10: 0.758
11: 0.668
12: 0.582
13: 0.55
14: 0.46
15: 0.4
16: 0.355
17: 0.304
18: 0.293
19: 0.269
20: 0.2
21: 0.219
22: 0.199
23: 0.168
24: 0.144
25: 0.134
26: 0.155
27: 0.111
28: 0.119
29: 0.119
30: 0.114
31: 0.091
32: 0.103
33: 0.069
34: 0.078
35: 0.086
36: 0.074
37: 0.084
38: 0.068
39: 0.054
40: 0.050
41: 0.059
42: 0.048
43: 0.037
44: 0.054
45: 0.050
46: 0.046
47: 0.043
48: 0.052
49: 0.038
50: 0.042
51: 0.046
52: 0.046
53: 0.045
54: 0.059
55: 0.048
56: 0.041
57: 0.042
58: 0.037
59: 0.035
60: 0.037
61: 0.038
62: 0.025
63: 0.039
64: 0.038
65: 0.030
66: 0.029
67: 0.028
68: 0.031
69: 0.033
70: 0.040
71: 0.028
72: 0.021
73: 0.031
74: 0.032
75: 0.026
76: 0.025
77: 0.030
78: 0.029
79: 0.020
80: 0.020
81: 0.030
82: 0.028
83: 0.017
84: 0.024
85: 0.021
86: 0.020
87: 0.036
88: 0.028
89: 0.020
90: 0.024
91: 0.016
92: 0.026
93: 0.018
94: 0.013
95: 0.021
96: 0.015
97: 0.022
98: 0.019
99: 0.018
100: 0.016
101: 0.018
102: 0.027
103: 0.033
104: 0.039
105: 0.039
106: 0.035
107: 0.034
108: 0.031
109: 0.037
110: 0.025
111: 0.018
112: 0.032
113: 0.024
114: 0.028
115: 0.021
116: 0.023
117: 0.029
118: 0.021
119: 0.020
120: 0.022
121: 0.017
122: 0.021
123: 0.016
124: 0.010
125: 0.011
126: 0.019
127: 0.005
128: 0.010
129: 0.012
130: 0.009
131: 0.009
132: 0.003
133: 0.011
134: 0.011
135: 0.006
136: 0.009
137: 0.008
138: 0.006
139: 0.007
140: 0.011
141: 0.006
142: 0.004
143: 0.007
144: 0.011
145: 0.006
146: 0.002
147: 0.006
148: 0.010
149: 0.004
150: 0.001
151: 0.005
152: 0.002
153: 0.004
154: 0.004
155: 0.001
156: 0.007
157: 0.006
158: 0.004
159: 0.006
160: 0.003
161: 0.002
162: 0.005
163: 0.003
164: 0.004
165: 0.001
167: 0.007
168: 0.005
169: 0.004
170: 0.003
171: 0.003
172: 0.007
173: 0.002
174: 0.006
175: 0.003
176: 0.004
177: 0.003
178: 0.004
179: 0.005
180: 0.005
181: 0.002
182: 0.001
183: 0.005
184: 0.002
185: 0.001
186: 0.003
188: 0.005
190: 0.004
191: 0.001
193: 0.004
194: 0.002
196: 0.002
197: 0.004
198: 0.005
199: 0.001
200: 0.002
201: 0.001
202: 0.002
203: 0.004
205: 0.003
206: 0.006
207: 0.007
208: 0.008
209: 0.006
210: 0.011
211: 0.013
212: 0.006
213: 0.007
214: 0.005
215: 0.014
216: 0.008
217: 0.006
218: 0.005
219: 0.008
220: 0.006
221: 0.005
222: 0.004
223: 0.005
224: 0.003
225: 0.008
226: 0.005
227: 0.003
228: 0.006
229: 0.008
230: 0.004
231: 0.002
233: 0.001
234: 0.005
236: 0.001
237: 0.003
238: 0.005
239: 0.003
240: 0.003
241: 0.002
242: 0.003
243: 0.001
244: 0.002
245: 0.003
246: 0.001
248: 0.002
249: 0.001
251: 0.001
253: 0.001
254: 0.001
255: 0.001
256: 0.001
257: 0.001
261: 0.001
265: 0.003
266: 0.003
267: 0.001
277: 0.001
278: 0.001
280: 0.002
282: 0.001
284: 0.002
291: 0.001
293: 0.001
294: 0.001
298: 0.001
303: 0.001
305: 0.001
307: 0.001
310: 0.001
313: 0.001
314: 0.002
315: 0.002
320: 0.001
322: 0.002
323: 0.001
324: 0.001
326: 0.001
332: 0.001
334: 0.003
336: 0.001
337: 0.001
343: 0.001
349: 0.002
351: 0.001
352: 0.001
354: 0.003
355: 0.001
356: 0.001
357: 0.001
362: 0.001
364: 0.001
366: 0.001
370: 0.002
371: 0.001
373: 0.001
381: 0.001
383: 0.001
392: 0.001
395: 0.002
396: 0.001
397: 0.001
401: 0.001
402: 0.001
403: 0.001
415: 0.001
416: 0.001
432: 0.001
433: 0.001
434: 0.001
445: 0.001
449: 0.001
451: 0.001
453: 0.001
455: 0.001
456: 0.001
457: 0.001
462: 0.001
465: 0.001
472: 0.001
473: 0.001
483: 0.001
489: 0.001
500: 0.001
508: 0.001
525: 0.001
526: 0.001
528: 0.001
529: 0.001
536: 0.002
538: 0.001
540: 0.001
550: 0.001
553: 0.001
561: 0.001
562: 0.001
567: 0.001
589: 0.001
594: 0.001
621: 0.001
622: 0.001
630: 0.001
632: 0.001
633: 0.001
640: 0.001
691: 0.001
708: 0.001
713: 0.001
720: 0.001
742: 0.002
755: 0.001
763: 0.001
823: 0.001
865: 0.001
934: 0.001
937: 0.001
965: 0.001
~: 68.73

--}