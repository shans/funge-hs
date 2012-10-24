-- what proportion of randomly generated [10]-Funges terminate within n cycles?

import FungeBase
import RunFunge
import RandomFunge
import Control.Monad
import Data.Maybe

run_1_core n = doRandomFunge (runFor n) [10] [0] [1] (defaultConfig { charSet = chars1D, trace = noTrace, acceptUserInput = False, activeExtensions = "" })
run_1 runs cycles = liftM ((* 100) . (/ fromIntegral runs) . fromIntegral . length . catMaybes . map fst) (sequence . replicate runs $ run_1_core cycles)

{--
 (most runs not completing due to 2 bugs)
 100 100 -> 28% 17% 23% 21% 
 (fixed some instances of chr (-n))
 16% 24%
 (fixed more. Still some hangs)
 25% 
 (non-exhaustive patterns for e.g. +_>'qk*;b5. Ah. Iterate 'Quit' is problematic because Quit is normally handled higher up. Fixed.)
 24% 24% 21% 25% 23% 
 (there's still a hang somewhere. For example, "9-c:pd{$j." hangs. Looks like it's because this jumps to -4 with a delta of 1, and I don't handle forward out-of-bounds yet. Fixed.)

 1000 100 -> 22.8% 22.5% 23.8% 
 (hah, ;_5f*d*u+; still hangs. It's a fair cop. Fixed by letting ';' take 1 cycle to execute.)
 20.2% 
 ("\"*5}7]9>jg" -> non exhaustive pattern in function toChar. Ah, no mapping for StringMode because I didn't think I'd need one :-))
 20.9% 22.2% 22.3% 22.7% 22.1%
 10000 100 -> 
 (hangs on "?3d5s2+gb+". Or doesn't. Hmm. Ah - occasionally an InputDecimal command is inserted and executed (randomness due to '?'). Added an option for accepting user input.)
 (hangs on "b}-{6 a,:9". This looks like it demonstrates 2 bugs. (1) "b}" shouldn't push into the stack frame as endBlock should have no effect on a single-entry stack stack.
     (2) EndBlock on [-1, 11, 11, 0, 0] [0, 0, ....] hangs. I suspect this is because of the -1 at the top of the stack. Both fixed.)
 ("-'p?%:+.\"{" -> Exception: Prelude.chr: bad argument (-45). Wait, shouldn't StringMode load the String on the stack? Shouldn't BeginBlock always push another stack frame?
 Ah, these are being traced in StringMode. So the issue is encountering the Reverse (-45) while in StringMode. Looks OK now.)
 (hangs on "]npw`k*pd\"". Actually this doesn't hang! It's just incredibly slow because it introduces a ridiculously sparse IP space.)
 (hangs on "}${*k/1'\"8". Again it doesn't.)
 22.04% 22.89%
 (crash because I ran out of swap. Oops.)
 ("*,?'*\"2wa*" -> Prelude.chr: bad argument: 2424030000. Largest 'chr' input is 1114111. Fix outputCharacter.
 ("?5bP*,r*;f" -> Prelude.chr: bad argument: 9397265625. Need to fix Print too. Print actually shouldn't be in the standard set of characters... removing. Can test
  sensitivity in a different run.)

 --- above results should be considered as extensionCharacters="P"
 10000 100 -> 22.82% 22.48% 22.67% 22.03% 
 ("\\}%uek*;d " seems to be unhappy. This is because it calls EndBlock with a *ludicrously* large value - 2177953337809371136. Frame size now limited to 1024.)
 22.4%
 100 100 -> 26% 
 ("-b3g>#k?cp" another chr error. Problem with decoding artificial Reverse instructions.
 
--}

{-- what about running for far fewer cycles?
 100 10 -> 19% 19% 20% 16% 15% 
 1000 10 -> 21.7% 19.3% 20% 20.6% 20.1%
 10000 10 -> 20.01% 20.62% 20.93% 20.46% 20.98%
--}

{-- let's try longer funges. --}

run_2_core n = doRandomFunge (runFor n) [20] [0] [1] (defaultConfig { charSet = chars1D, trace = noTrace, acceptUserInput = False, activeExtensions = "" })
run_2 runs cycles = liftM ((* 100) . (/ fromIntegral runs) . fromIntegral . length . catMaybes . map fst) (sequence . replicate runs $ run_2_core cycles)

{--
  100 100 -> 29% 17% 30% 29% 21% 
  1000 100 -> 25.8% 28% 28%  
  ( "fa}$9,[r13u$\"/k*pn;4" JumpOver, empty stack, huge bounds. I think to support this kind of thing we'll need sparse ranges. I think to do *that* I'll want a range module with
   tests.)
  24.8% 29.1% 26.6%
  10000 100 -> 
  ( "k+/[/n7k*ka. _{}{\"6-" - *massive* iterate value. Probably want to limit this too? Actually I could probably implement most large iterates with a more sophisticated stack and 
    a special "iterate" version of each instruction. For now I'm limiting to 1024.)
  27.99% 27.46% 
  ("\"{pp*<6`0q;e-6b['?ds" is slow. Hah. Need to prevent massive negative ranges too :-)
  26.55% 27.13% 
  ("{\"?#@'fa$u3`'>g}*]ws" is slow. EndBlock needs to deal with large negative ranges as well. Sigh.
  27.71% 26.76% 26.91% 27.78% 28.46%
--}

{-- 10000 10 -> 22.09% 21.32% 19.72% 21.53% 21.26%
    100000 9 -> 20.694%
    10000 9 -> 20.60% 21.06% 19.45% 20.66% 19.91%
    10000 8 -> 19.68% 19.44% 19.90% 19.53% 20.02%
    10000 7 -> 18.17% 19.35% 18.47% 19.30% 18.87%
    10000 6 -> 17.96% 17.58% 17.91% 18.20% 17.75%
    10000 5 -> 15.65% 15.76% 16.76% 16.17% 16.41%
--} 
