(* The Haskell Research Compiler *)
(*
 * Redistribution and use in source and binary forms, with or without modification, are permitted 
 * provided that the following conditions are met:
 * 1.   Redistributions of source code must retain the above copyright notice, this list of 
 * conditions and the following disclaimer.
 * 2.   Redistributions in binary form must reproduce the above copyright notice, this list of
 * conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING,
 * BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY
 * OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING
 * IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *)


signature FLAT_TUPLE = 
sig
  val flatten1 : 'a1
                 -> 'a1
  val nest1 : 'a1
              -> 'a1
                 
  val flatten2 : ('a2 * 'a1) 
                 -> ('a2 * 'a1)
  val nest2 : ('a2 * 'a1) 
              -> ('a2 * 'a1)
                 
  val flatten3 : ('a3 * ('a2 * 'a1)) 
                 -> ('a3 * 'a2 * 'a1)
  val nest3 : ('a3 * 'a2 * 'a1) 
              -> ('a3 * ('a2 * 'a1))

  val flatten4 : ('a4 * ('a3 * ('a2 * 'a1))) 
                 -> ('a4 * 'a3 * 'a2 * 'a1)
  val nest4 : ('a4 * 'a3 * 'a2 * 'a1) 
              -> ('a4 * ('a3 * ('a2 * 'a1)))

  val flatten5 : ('a5 * ('a4 * ('a3 * ('a2 * 'a1)))) 
                 -> ('a5 * 'a4 * 'a3 * 'a2 * 'a1)
  val nest5 : ('a5 * 'a4 * 'a3 * 'a2 * 'a1) 
              -> ('a5 * ('a4 * ('a3 * ('a2 * 'a1))))

  val flatten6 : ('a6 * ('a5 * ('a4 * ('a3 * ('a2 * 'a1))))) 
                 -> ('a6 * 'a5 * 'a4 * 'a3 * 'a2 * 'a1)
  val nest6 : ('a6 * 'a5 * 'a4 * 'a3 * 'a2 * 'a1) 
              -> ('a6 * ('a5 * ('a4 * ('a3 * ('a2 * 'a1)))))

  val flatten7 : ('a7 * ('a6 * ('a5 * ('a4 * ('a3 * ('a2 * 'a1)))))) 
                 -> ('a7 * 'a6 * 'a5 * 'a4 * 'a3 * 'a2 * 'a1)
  val nest7 : ('a7 * 'a6 * 'a5 * 'a4 * 'a3 * 'a2 * 'a1) 
              -> ('a7 * ('a6 * ('a5 * ('a4 * ('a3 * ('a2 * 'a1))))))

  val flatten8 : ('a8 * ('a7 * ('a6 * ('a5 * ('a4 * ('a3 * ('a2 * 'a1))))))) 
                 -> ('a8 * 'a7 * 'a6 * 'a5 * 'a4 * 'a3 * 'a2 * 'a1)
  val nest8 : ('a8 * 'a7 * 'a6 * 'a5 * 'a4 * 'a3 * 'a2 * 'a1) 
              -> ('a8 * ('a7 * ('a6 * ('a5 * ('a4 * ('a3 * ('a2 * 'a1)))))))

  val flatten9 : ('a9 * ('a8 * ('a7 * ('a6 * ('a5 * ('a4 * ('a3 * ('a2 * 'a1)))))))) 
                 -> ('a9 * 'a8 * 'a7 * 'a6 * 'a5 * 'a4 * 'a3 * 'a2 * 'a1)
  val nest9 : ('a9 * 'a8 * 'a7 * 'a6 * 'a5 * 'a4 * 'a3 * 'a2 * 'a1) 
              -> ('a9 * ('a8 * ('a7 * ('a6 * ('a5 * ('a4 * ('a3 * ('a2 * 'a1))))))))

  val flatten10 : ('a10 * ('a9 * ('a8 * ('a7 * ('a6 * ('a5 * ('a4 * ('a3 * ('a2 * 'a1))))))))) 
                  -> ('a10 * 'a9 * 'a8 * 'a7 * 'a6 * 'a5 * 'a4 * 'a3 * 'a2 * 'a1)
  val nest10 : ('a10 * 'a9 * 'a8 * 'a7 * 'a6 * 'a5 * 'a4 * 'a3 * 'a2 * 'a1) 
               -> ('a10 * ('a9 * ('a8 * ('a7 * ('a6 * ('a5 * ('a4 * ('a3 * ('a2 * 'a1)))))))))

  val flatten11 : ('a11 * ('a10 * ('a9 * ('a8 * ('a7 * ('a6 * ('a5 * ('a4 * ('a3 * ('a2 * 'a1)))))))))) 
                  -> ('a11 * 'a10 * 'a9 * 'a8 * 'a7 * 'a6 * 'a5 * 'a4 * 'a3 * 'a2 * 'a1)
  val nest11 : ('a11 * 'a10 * 'a9 * 'a8 * 'a7 * 'a6 * 'a5 * 'a4 * 'a3 * 'a2 * 'a1) 
               -> ('a11 * ('a10 * ('a9 * ('a8 * ('a7 * ('a6 * ('a5 * ('a4 * ('a3 * ('a2 * 'a1))))))))))

  val flatten12 : ('a12 * ('a11 * ('a10 * ('a9 * ('a8 * ('a7 * ('a6 * ('a5 * ('a4 * ('a3 * ('a2 * 'a1))))))))))) 
                  -> ('a12 * 'a11 * 'a10 * 'a9 * 'a8 * 'a7 * 'a6 * 'a5 * 'a4 * 'a3 * 'a2 * 'a1)
  val nest12 : ('a12 * 'a11 * 'a10 * 'a9 * 'a8 * 'a7 * 'a6 * 'a5 * 'a4 * 'a3 * 'a2 * 'a1) 
               -> ('a12 * ('a11 * ('a10 * ('a9 * ('a8 * ('a7 * ('a6 * ('a5 * ('a4 * ('a3 * ('a2 * 'a1)))))))))))

  val flatten13 : ('a13 * ('a12 * ('a11 * ('a10 * ('a9 * ('a8 * ('a7 * ('a6 * ('a5 * ('a4 * ('a3 * ('a2 
                        * 'a1)))))))))))) 
                  -> ('a13 * 'a12 * 'a11 * 'a10 * 'a9 * 'a8 * 'a7 * 'a6 * 'a5 * 'a4 * 'a3 * 'a2 * 'a1)
  val nest13 : ('a13 * 'a12 * 'a11 * 'a10 * 'a9 * 'a8 * 'a7 * 'a6 * 'a5 * 'a4 * 'a3 * 'a2 * 'a1) 
               -> ('a13 * ('a12 * ('a11 * ('a10 * ('a9 * ('a8 * ('a7 * ('a6 * ('a5 * ('a4 * ('a3 * ('a2 
                        * 'a1))))))))))))

  val flatten14 : ('a14 * ('a13 * ('a12 * ('a11 * ('a10 * ('a9 * ('a8 * ('a7 * ('a6 * ('a5 * ('a4 * ('a3 * ('a2 
                        * 'a1))))))))))))) 
                  -> ('a14 * 'a13 * 'a12 * 'a11 * 'a10 * 'a9 * 'a8 * 'a7 * 'a6 * 'a5 * 'a4 * 'a3 * 'a2 * 'a1)
  val nest14 : ('a14 * 'a13 * 'a12 * 'a11 * 'a10 * 'a9 * 'a8 * 'a7 * 'a6 * 'a5 * 'a4 * 'a3 * 'a2 * 'a1) 
               -> ('a14 * ('a13 * ('a12 * ('a11 * ('a10 * ('a9 * ('a8 * ('a7 * ('a6 * ('a5 * ('a4 * ('a3 * ('a2 
                        * 'a1)))))))))))))

  val flatten15 : ('a15 * ('a14 * ('a13 * ('a12 * ('a11 * ('a10 * ('a9 * ('a8 * ('a7 * ('a6 * ('a5 * ('a4 * ('a3 * ('a2 
                        * 'a1)))))))))))))) 
                  -> ('a15 * 'a14 * 'a13 * 'a12 * 'a11 * 'a10 * 'a9 * 'a8 * 'a7 * 'a6 * 'a5 * 'a4 * 'a3 * 'a2 * 'a1)
  val nest15 : ('a15 * 'a14 * 'a13 * 'a12 * 'a11 * 'a10 * 'a9 * 'a8 * 'a7 * 'a6 * 'a5 * 'a4 * 'a3 * 'a2 * 'a1) 
               -> ('a15 * ('a14 * ('a13 * ('a12 * ('a11 * ('a10 * ('a9 * ('a8 * ('a7 * ('a6 * ('a5 * ('a4 * ('a3 * ('a2 
                        * 'a1))))))))))))))

  val flatten16 : ('a16 * ('a15 * ('a14 * ('a13 * ('a12 * ('a11 * ('a10 * ('a9 * ('a8 * ('a7 * ('a6 * ('a5 * ('a4 
                        * ('a3 * ('a2 * 'a1))))))))))))))) 
                  -> ('a16 * 'a15 * 'a14 * 'a13 * 'a12 * 'a11 * 'a10 * 'a9 * 'a8 * 'a7 * 'a6 * 'a5 * 'a4 * 'a3 * 'a2 
                           * 'a1)
  val nest16 : ('a16 * 'a15 * 'a14 * 'a13 * 'a12 * 'a11 * 'a10 * 'a9 * 'a8 * 'a7 * 'a6 * 'a5 * 'a4 * 'a3 * 'a2 * 'a1) 
               -> ('a16 * ('a15 * ('a14 * ('a13 * ('a12 * ('a11 * ('a10 * ('a9 * ('a8 * ('a7 * ('a6 * ('a5 * ('a4 
                        * ('a3 * ('a2 * 'a1)))))))))))))))

  val flatten17 : ('a17 * ('a16 * ('a15 * ('a14 * ('a13 * ('a12 * ('a11 * ('a10 * ('a9 * ('a8 * ('a7 * ('a6 * ('a5 
                        * ('a4 * ('a3 * ('a2 * 'a1)))))))))))))))) 
                  -> ('a17 * 'a16 * 'a15 * 'a14 * 'a13 * 'a12 * 'a11 * 'a10 * 'a9 * 'a8 * 'a7 * 'a6 * 'a5 * 'a4 * 'a3 
                           * 'a2 * 'a1)
  val nest17 : ('a17 * 'a16 * 'a15 * 'a14 * 'a13 * 'a12 * 'a11 * 'a10 * 'a9 * 'a8 * 'a7 * 'a6 * 'a5 * 'a4 * 'a3 * 'a2 
                     * 'a1) 
               -> ('a17 * ('a16 * ('a15 * ('a14 * ('a13 * ('a12 * ('a11 * ('a10 * ('a9 * ('a8 * ('a7 * ('a6 * ('a5 
                        * ('a4 * ('a3 * ('a2 * 'a1))))))))))))))))

  val flatten18 : ('a18 * ('a17 * ('a16 * ('a15 * ('a14 * ('a13 * ('a12 * ('a11 * ('a10 * ('a9 * ('a8 * ('a7 * ('a6 
                        * ('a5 * ('a4 * ('a3 * ('a2 * 'a1))))))))))))))))) 
                  -> ('a18 * 'a17 * 'a16 * 'a15 * 'a14 * 'a13 * 'a12 * 'a11 * 'a10 * 'a9 * 'a8 * 'a7 * 'a6 * 'a5 * 'a4 
                           * 'a3 * 'a2 * 'a1)
  val nest18 : ('a18 * 'a17 * 'a16 * 'a15 * 'a14 * 'a13 * 'a12 * 'a11 * 'a10 * 'a9 * 'a8 * 'a7 * 'a6 * 'a5 * 'a4 * 'a3 
                     * 'a2 * 'a1) 
               -> ('a18 * ('a17 * ('a16 * ('a15 * ('a14 * ('a13 * ('a12 * ('a11 * ('a10 * ('a9 * ('a8 * ('a7 * ('a6 
                        * ('a5 * ('a4 * ('a3 * ('a2 * 'a1)))))))))))))))))

  val flatten19 : ('a19 * ('a18 * ('a17 * ('a16 * ('a15 * ('a14 * ('a13 * ('a12 * ('a11 * ('a10 * ('a9 * ('a8 * ('a7 
                        * ('a6 * ('a5 * ('a4 * ('a3 * ('a2 * 'a1)))))))))))))))))) 
                  -> ('a19 * 'a18 * 'a17 * 'a16 * 'a15 * 'a14 * 'a13 * 'a12 * 'a11 * 'a10 * 'a9 * 'a8 * 'a7 * 'a6 * 'a5 
                           * 'a4 * 'a3 * 'a2 * 'a1)
  val nest19 : ('a19 * 'a18 * 'a17 * 'a16 * 'a15 * 'a14 * 'a13 * 'a12 * 'a11 * 'a10 * 'a9 * 'a8 * 'a7 * 'a6 * 'a5 * 'a4 
                     * 'a3 * 'a2 * 'a1) 
               -> ('a19 * ('a18 * ('a17 * ('a16 * ('a15 * ('a14 * ('a13 * ('a12 * ('a11 * ('a10 * ('a9 * ('a8 * ('a7 
                     * ('a6 * ('a5 * ('a4 * ('a3 * ('a2 * 'a1))))))))))))))))))

  val flatten20 : ('a20 * ('a19 * ('a18 * ('a17 * ('a16 * ('a15 * ('a14 * ('a13 * ('a12 * ('a11 * ('a10 * ('a9 * ('a8 
                        * ('a7 * ('a6 * ('a5 * ('a4 * ('a3 * ('a2 * 'a1))))))))))))))))))) 
                  -> ('a20 * 'a19 * 'a18 * 'a17 * 'a16 * 'a15 * 'a14 * 'a13 * 'a12 * 'a11 * 'a10 * 'a9 * 'a8 * 'a7 
                           * 'a6 * 'a5 * 'a4 * 'a3 * 'a2 * 'a1)
  val nest20 : ('a20 * 'a19 * 'a18 * 'a17 * 'a16 * 'a15 * 'a14 * 'a13 * 'a12 * 'a11 * 'a10 * 'a9 * 'a8 * 'a7 * 'a6 
                     * 'a5 * 'a4 * 'a3 * 'a2 * 'a1) 
               -> ('a20 * ('a19 * ('a18 * ('a17 * ('a16 * ('a15 * ('a14 * ('a13 * ('a12 * ('a11 * ('a10 * ('a9 * ('a8 
                        * ('a7 * ('a6 * ('a5 * ('a4 * ('a3 * ('a2 * 'a1)))))))))))))))))))

  val flatten21 : ('a21 * ('a20 * ('a19 * ('a18 * ('a17 * ('a16 * ('a15 * ('a14 * ('a13 * ('a12 * ('a11 * ('a10 * ('a9 
                        * ('a8 * ('a7 * ('a6 * ('a5 * ('a4 * ('a3 * ('a2 * 'a1)))))))))))))))))))) 
                  -> ('a21 * 'a20 * 'a19 * 'a18 * 'a17 * 'a16 * 'a15 * 'a14 * 'a13 * 'a12 * 'a11 * 'a10 * 'a9 * 'a8 
                           * 'a7 * 'a6 * 'a5 * 'a4 * 'a3 * 'a2 * 'a1)
  val nest21 : ('a21 * 'a20 * 'a19 * 'a18 * 'a17 * 'a16 * 'a15 * 'a14 * 'a13 * 'a12 * 'a11 * 'a10 * 'a9 * 'a8 * 'a7 
                     * 'a6 * 'a5 * 'a4 * 'a3 * 'a2 * 'a1) 
               -> ('a21 * ('a20 * ('a19 * ('a18 * ('a17 * ('a16 * ('a15 * ('a14 * ('a13 * ('a12 * ('a11 * ('a10 * ('a9 
                     * ('a8 * ('a7 * ('a6 * ('a5 * ('a4 * ('a3 * ('a2 * 'a1))))))))))))))))))))

  val flatten22 : ('a22 * ('a21 * ('a20 * ('a19 * ('a18 * ('a17 * ('a16 * ('a15 * ('a14 * ('a13 * ('a12 * ('a11 * ('a10 
                        * ('a9 * ('a8 * ('a7 * ('a6 * ('a5 * ('a4 * ('a3 * ('a2 * 'a1))))))))))))))))))))) 
                  -> ('a22 * 'a21 * 'a20 * 'a19 * 'a18 * 'a17 * 'a16 * 'a15 * 'a14 * 'a13 * 'a12 * 'a11 * 'a10 * 'a9 
                           * 'a8 * 'a7 * 'a6 * 'a5 * 'a4 * 'a3 * 'a2 * 'a1)
  val nest22 : ('a22 * 'a21 * 'a20 * 'a19 * 'a18 * 'a17 * 'a16 * 'a15 * 'a14 * 'a13 * 'a12 * 'a11 * 'a10 * 'a9 * 'a8 
                     * 'a7 * 'a6 * 'a5 * 'a4 * 'a3 * 'a2 * 'a1) 
               -> ('a22 * ('a21 * ('a20 * ('a19 * ('a18 * ('a17 * ('a16 * ('a15 * ('a14 * ('a13 * ('a12 * ('a11 * ('a10 
                        * ('a9 * ('a8 * ('a7 * ('a6 * ('a5 * ('a4 * ('a3 * ('a2 * 'a1)))))))))))))))))))))

  val flatten23 : ('a23 * ('a22 * ('a21 * ('a20 * ('a19 * ('a18 * ('a17 * ('a16 * ('a15 * ('a14 * ('a13 * ('a12 * ('a11 
                        * ('a10 * ('a9 * ('a8 * ('a7 * ('a6 * ('a5 * ('a4 * ('a3 * ('a2 * 'a1)))))))))))))))))))))) 
                  -> ('a23 * 'a22 * 'a21 * 'a20 * 'a19 * 'a18 * 'a17 * 'a16 * 'a15 * 'a14 * 'a13 * 'a12 * 'a11 * 'a10 
                           * 'a9 * 'a8 * 'a7 * 'a6 * 'a5 * 'a4 * 'a3 * 'a2 * 'a1)
  val nest23 : ('a23 * 'a22 * 'a21 * 'a20 * 'a19 * 'a18 * 'a17 * 'a16 * 'a15 * 'a14 * 'a13 * 'a12 * 'a11 * 'a10 * 'a9 
                     * 'a8 * 'a7 * 'a6 * 'a5 * 'a4 * 'a3 * 'a2 * 'a1) 
               -> ('a23 * ('a22 * ('a21 * ('a20 * ('a19 * ('a18 * ('a17 * ('a16 * ('a15 * ('a14 * ('a13 * ('a12 * ('a11 
                        * ('a10 * ('a9 * ('a8 * ('a7 * ('a6 * ('a5 * ('a4 * ('a3 * ('a2 * 'a1))))))))))))))))))))))

  val flatten24 : ('a24 * ('a23 * ('a22 * ('a21 * ('a20 * ('a19 * ('a18 * ('a17 * ('a16 * ('a15 * ('a14 * ('a13 * ('a12 
                        * ('a11 * ('a10 * ('a9 * ('a8 * ('a7 * ('a6 * ('a5 * ('a4 * ('a3 * ('a2 
                        * 'a1))))))))))))))))))))))) 
                  -> ('a24 * 'a23 * 'a22 * 'a21 * 'a20 * 'a19 * 'a18 * 'a17 * 'a16 * 'a15 * 'a14 * 'a13 * 'a12 * 'a11 
                           * 'a10 * 'a9 * 'a8 * 'a7 * 'a6 * 'a5 * 'a4 * 'a3 * 'a2 * 'a1)
  val nest24 : ('a24 * 'a23 * 'a22 * 'a21 * 'a20 * 'a19 * 'a18 * 'a17 * 'a16 * 'a15 * 'a14 * 'a13 * 'a12 * 'a11 * 'a10 
                     * 'a9 * 'a8 * 'a7 * 'a6 * 'a5 * 'a4 * 'a3 * 'a2 * 'a1) 
               -> ('a24 * ('a23 * ('a22 * ('a21 * ('a20 * ('a19 * ('a18 * ('a17 * ('a16 * ('a15 * ('a14 * ('a13 * ('a12 
                        * ('a11 * ('a10 * ('a9 * ('a8 * ('a7 * ('a6 * ('a5 * ('a4 * ('a3 * ('a2 
                        * 'a1)))))))))))))))))))))))

  val flatten25 : ('a25 * ('a24 * ('a23 * ('a22 * ('a21 * ('a20 * ('a19 * ('a18 * ('a17 * ('a16 * ('a15 * ('a14 * ('a13 
                        * ('a12 * ('a11 * ('a10 * ('a9 * ('a8 * ('a7 * ('a6 * ('a5 * ('a4 * ('a3 * ('a2 
                        * 'a1)))))))))))))))))))))))) 
                  -> ('a25 * 'a24 * 'a23 * 'a22 * 'a21 * 'a20 * 'a19 * 'a18 * 'a17 * 'a16 * 'a15 * 'a14 * 'a13 * 'a12 
                           * 'a11 * 'a10 * 'a9 * 'a8 * 'a7 * 'a6 * 'a5 * 'a4 * 'a3 * 'a2 * 'a1)
  val nest25 : ('a25 * 'a24 * 'a23 * 'a22 * 'a21 * 'a20 * 'a19 * 'a18 * 'a17 * 'a16 * 'a15 * 'a14 * 'a13 * 'a12 * 'a11 
                     * 'a10 * 'a9 * 'a8 * 'a7 * 'a6 * 'a5 * 'a4 * 'a3 * 'a2 * 'a1) 
               -> ('a25 * ('a24 * ('a23 * ('a22 * ('a21 * ('a20 * ('a19 * ('a18 * ('a17 * ('a16 * ('a15 * ('a14 * ('a13 
                        * ('a12 * ('a11 * ('a10 * ('a9 * ('a8 * ('a7 * ('a6 * ('a5 * ('a4 * ('a3 * ('a2 
                        * 'a1))))))))))))))))))))))))

  val flatten26 : ('a26 * ('a25 * ('a24 * ('a23 * ('a22 * ('a21 * ('a20 * ('a19 * ('a18 * ('a17 * ('a16 * ('a15 * ('a14 
                        * ('a13 * ('a12 * ('a11 * ('a10 * ('a9 * ('a8 * ('a7 * ('a6 * ('a5 * ('a4 * ('a3 * ('a2 
                        * 'a1))))))))))))))))))))))))) 
                  -> ('a26 * 'a25 * 'a24 * 'a23 * 'a22 * 'a21 * 'a20 * 'a19 * 'a18 * 'a17 * 'a16 * 'a15 * 'a14 * 'a13 
                           * 'a12 * 'a11 * 'a10 * 'a9 * 'a8 * 'a7 * 'a6 * 'a5 * 'a4 * 'a3 * 'a2 * 'a1)
  val nest26 : ('a26 * 'a25 * 'a24 * 'a23 * 'a22 * 'a21 * 'a20 * 'a19 * 'a18 * 'a17 * 'a16 * 'a15 * 'a14 * 'a13 * 'a12 
                     * 'a11 * 'a10 * 'a9 * 'a8 * 'a7 * 'a6 * 'a5 * 'a4 * 'a3 * 'a2 * 'a1) 
               -> ('a26 * ('a25 * ('a24 * ('a23 * ('a22 * ('a21 * ('a20 * ('a19 * ('a18 * ('a17 * ('a16 * ('a15 * ('a14 
                        * ('a13 * ('a12 * ('a11 * ('a10 * ('a9 * ('a8 * ('a7 * ('a6 * ('a5 * ('a4 * ('a3 * ('a2 
                        * 'a1)))))))))))))))))))))))))

  val flatten27 : ('a27 * ('a26 * ('a25 * ('a24 * ('a23 * ('a22 * ('a21 * ('a20 * ('a19 * ('a18 * ('a17 * ('a16 * ('a15 
                        * ('a14 * ('a13 * ('a12 * ('a11 * ('a10 * ('a9 * ('a8 * ('a7 * ('a6 * ('a5 * ('a4 * ('a3 * ('a2 
                        * 'a1)))))))))))))))))))))))))) 
                  -> ('a27 * 'a26 * 'a25 * 'a24 * 'a23 * 'a22 * 'a21 * 'a20 * 'a19 * 'a18 * 'a17 * 'a16 * 'a15 * 'a14 
                           * 'a13 * 'a12 * 'a11 * 'a10 * 'a9 * 'a8 * 'a7 * 'a6 * 'a5 * 'a4 * 'a3 * 'a2 * 'a1)
  val nest27 : ('a27 * 'a26 * 'a25 * 'a24 * 'a23 * 'a22 * 'a21 * 'a20 * 'a19 * 'a18 * 'a17 * 'a16 * 'a15 * 'a14 * 'a13 
                     * 'a12 * 'a11 * 'a10 * 'a9 * 'a8 * 'a7 * 'a6 * 'a5 * 'a4 * 'a3 * 'a2 * 'a1) 
               -> ('a27 * ('a26 * ('a25 * ('a24 * ('a23 * ('a22 * ('a21 * ('a20 * ('a19 * ('a18 * ('a17 * ('a16 * ('a15 
                        * ('a14 * ('a13 * ('a12 * ('a11 * ('a10 * ('a9 * ('a8 * ('a7 * ('a6 * ('a5 * ('a4 * ('a3 * ('a2 
                        * 'a1))))))))))))))))))))))))))

  val flatten28 : ('a28 * ('a27 * ('a26 * ('a25 * ('a24 * ('a23 * ('a22 * ('a21 * ('a20 * ('a19 * ('a18 * ('a17 * ('a16 
                        * ('a15 * ('a14 * ('a13 * ('a12 * ('a11 * ('a10 * ('a9 * ('a8 * ('a7 * ('a6 * ('a5 * ('a4 
                        * ('a3 * ('a2 * 'a1))))))))))))))))))))))))))) 
                  -> ('a28 * 'a27 * 'a26 * 'a25 * 'a24 * 'a23 * 'a22 * 'a21 * 'a20 * 'a19 * 'a18 * 'a17 * 'a16 * 'a15 
                           * 'a14 * 'a13 * 'a12 * 'a11 * 'a10 * 'a9 * 'a8 * 'a7 * 'a6 * 'a5 * 'a4 * 'a3 * 'a2 * 'a1)
  val nest28 : ('a28 * 'a27 * 'a26 * 'a25 * 'a24 * 'a23 * 'a22 * 'a21 * 'a20 * 'a19 * 'a18 * 'a17 * 'a16 * 'a15 * 'a14 
                     * 'a13 * 'a12 * 'a11 * 'a10 * 'a9 * 'a8 * 'a7 * 'a6 * 'a5 * 'a4 * 'a3 * 'a2 * 'a1) 
               -> ('a28 * ('a27 * ('a26 * ('a25 * ('a24 * ('a23 * ('a22 * ('a21 * ('a20 * ('a19 * ('a18 * ('a17 * ('a16 
                        * ('a15 * ('a14 * ('a13 * ('a12 * ('a11 * ('a10 * ('a9 * ('a8 * ('a7 * ('a6 * ('a5 * ('a4 
                        * ('a3 * ('a2 * 'a1)))))))))))))))))))))))))))

  val flatten29 : ('a29 * ('a28 * ('a27 * ('a26 * ('a25 * ('a24 * ('a23 * ('a22 * ('a21 * ('a20 * ('a19 * ('a18 * ('a17 
                        * ('a16 * ('a15 * ('a14 * ('a13 * ('a12 * ('a11 * ('a10 * ('a9 * ('a8 * ('a7 * ('a6 * ('a5 
                        * ('a4 * ('a3 * ('a2 * 'a1)))))))))))))))))))))))))))) 
                  -> ('a29 * 'a28 * 'a27 * 'a26 * 'a25 * 'a24 * 'a23 * 'a22 * 'a21 * 'a20 * 'a19 * 'a18 * 'a17 * 'a16 
                           * 'a15 * 'a14 * 'a13 * 'a12 * 'a11 * 'a10 * 'a9 * 'a8 * 'a7 * 'a6 * 'a5 * 'a4 * 'a3 * 'a2 
                           * 'a1)
  val nest29 : ('a29 * 'a28 * 'a27 * 'a26 * 'a25 * 'a24 * 'a23 * 'a22 * 'a21 * 'a20 * 'a19 * 'a18 * 'a17 * 'a16 * 'a15 
                     * 'a14 * 'a13 * 'a12 * 'a11 * 'a10 * 'a9 * 'a8 * 'a7 * 'a6 * 'a5 * 'a4 * 'a3 * 'a2 * 'a1) 
               -> ('a29 * ('a28 * ('a27 * ('a26 * ('a25 * ('a24 * ('a23 * ('a22 * ('a21 * ('a20 * ('a19 * ('a18 * ('a17 
                        * ('a16 * ('a15 * ('a14 * ('a13 * ('a12 * ('a11 * ('a10 * ('a9 * ('a8 * ('a7 * ('a6 * ('a5 
                        * ('a4 * ('a3 * ('a2 * 'a1))))))))))))))))))))))))))))

  val flatten30 : ('a30 * ('a29 * ('a28 * ('a27 * ('a26 * ('a25 * ('a24 * ('a23 * ('a22 * ('a21 * ('a20 * ('a19 * ('a18 
                        * ('a17 * ('a16 * ('a15 * ('a14 * ('a13 * ('a12 * ('a11 * ('a10 * ('a9 * ('a8 * ('a7 * ('a6 
                        * ('a5 * ('a4 * ('a3 * ('a2 * 'a1))))))))))))))))))))))))))))) 
                  -> ('a30 * 'a29 * 'a28 * 'a27 * 'a26 * 'a25 * 'a24 * 'a23 * 'a22 * 'a21 * 'a20 * 'a19 * 'a18 * 'a17 
                           * 'a16 * 'a15 * 'a14 * 'a13 * 'a12 * 'a11 * 'a10 * 'a9 * 'a8 * 'a7 * 'a6 * 'a5 * 'a4 * 'a3 
                           * 'a2 * 'a1)
  val nest30 : ('a30 * 'a29 * 'a28 * 'a27 * 'a26 * 'a25 * 'a24 * 'a23 * 'a22 * 'a21 * 'a20 * 'a19 * 'a18 * 'a17 * 'a16 
                     * 'a15 * 'a14 * 'a13 * 'a12 * 'a11 * 'a10 * 'a9 * 'a8 * 'a7 * 'a6 * 'a5 * 'a4 * 'a3 * 'a2 * 'a1) 
               -> ('a30 * ('a29 * ('a28 * ('a27 * ('a26 * ('a25 * ('a24 * ('a23 * ('a22 * ('a21 * ('a20 * ('a19 * ('a18 
                        * ('a17 * ('a16 * ('a15 * ('a14 * ('a13 * ('a12 * ('a11 * ('a10 * ('a9 * ('a8 * ('a7 * ('a6 
                        * ('a5 * ('a4 * ('a3 * ('a2 * 'a1)))))))))))))))))))))))))))))

end (* signature FLAT_TUPLE *)

structure FlatTuple :> FLAT_TUPLE =
struct
  val flatten1 = 
   fn (a1) => (a1)
  val nest1 = 
   fn (a1) => (a1)

  val flatten2 = 
   fn (a2, a1) =>
      (a2, a1)
  val nest2 = 
   fn (a2, a1) =>
      (a2, a1)

  val flatten3 = 
   fn (a3, (a2, a1)) =>
      (a3, a2, a1)
  val nest3 = 
   fn (a3, a2, a1) =>
      (a3, (a2, a1))

  val flatten4 = 
   fn (a4, (a3, (a2, a1))) =>
      (a4, a3, a2, a1)
  val nest4 = 
   fn (a4, a3, a2, a1) =>
      (a4, (a3, (a2, a1)))

  val flatten5 = 
   fn (a5, (a4, (a3, (a2, a1)))) =>
      (a5, a4, a3, a2, a1)
  val nest5 = 
   fn (a5, a4, a3, a2, a1) =>
      (a5, (a4, (a3, (a2, a1))))

  val flatten6 = 
   fn (a6, (a5, (a4, (a3, (a2, a1))))) =>
      (a6, a5, a4, a3, a2, a1)
  val nest6 = 
   fn (a6, a5, a4, a3, a2, a1) =>
      (a6, (a5, (a4, (a3, (a2, a1)))))

  val flatten7 = 
   fn (a7, (a6, (a5, (a4, (a3, (a2, a1)))))) =>
      (a7, a6, a5, a4, a3, a2, a1)
  val nest7 = 
   fn (a7, a6, a5, a4, a3, a2, a1) =>
      (a7, (a6, (a5, (a4, (a3, (a2, a1))))))

  val flatten8 = 
   fn (a8, (a7, (a6, (a5, (a4, (a3, (a2, a1))))))) =>
      (a8, a7, a6, a5, a4, a3, a2, a1)
  val nest8 = 
   fn (a8, a7, a6, a5, a4, a3, a2, a1) =>
      (a8, (a7, (a6, (a5, (a4, (a3, (a2, a1)))))))

  val flatten9 = 
   fn (a9, (a8, (a7, (a6, (a5, (a4, (a3, (a2, a1)))))))) =>
      (a9, a8, a7, a6, a5, a4, a3, a2, a1)
  val nest9 = 
   fn (a9, a8, a7, a6, a5, a4, a3, a2, a1) =>
      (a9, (a8, (a7, (a6, (a5, (a4, (a3, (a2, a1))))))))

  val flatten10 = 
   fn (a10, (a9, (a8, (a7, (a6, (a5, (a4, (a3, (a2, a1))))))))) =>
      (a10, a9, a8, a7, a6, a5, a4, a3, a2, a1)
  val nest10 = 
   fn (a10, a9, a8, a7, a6, a5, a4, a3, a2, a1) =>
      (a10, (a9, (a8, (a7, (a6, (a5, (a4, (a3, (a2, a1)))))))))

  val flatten11 = 
   fn (a11, (a10, (a9, (a8, (a7, (a6, (a5, (a4, (a3, (a2, a1)))))))))) =>
      (a11, a10, a9, a8, a7, a6, a5, a4, a3, a2, a1)
  val nest11 = 
   fn (a11, a10, a9, a8, a7, a6, a5, a4, a3, a2, a1) =>
      (a11, (a10, (a9, (a8, (a7, (a6, (a5, (a4, (a3, (a2, a1))))))))))

  val flatten12 = 
   fn (a12, (a11, (a10, (a9, (a8, (a7, (a6, (a5, (a4, (a3, (a2, a1))))))))))) =>
      (a12, a11, a10, a9, a8, a7, a6, a5, a4, a3, a2, a1)
  val nest12 = 
   fn (a12, a11, a10, a9, a8, a7, a6, a5, a4, a3, a2, a1) =>
      (a12, (a11, (a10, (a9, (a8, (a7, (a6, (a5, (a4, (a3, (a2, a1)))))))))))

  val flatten13 = 
   fn (a13, (a12, (a11, (a10, (a9, (a8, (a7, (a6, (a5, (a4, (a3, (a2, a1)))))))))))) =>
      (a13, a12, a11, a10, a9, a8, a7, a6, a5, a4, a3, a2, a1)
  val nest13 = 
   fn (a13, a12, a11, a10, a9, a8, a7, a6, a5, a4, a3, a2, a1) =>
      (a13, (a12, (a11, (a10, (a9, (a8, (a7, (a6, (a5, (a4, (a3, (a2, a1))))))))))))

  val flatten14 = 
   fn (a14, (a13, (a12, (a11, (a10, (a9, (a8, (a7, (a6, (a5, (a4, (a3, (a2, a1))))))))))))) =>
      (a14, a13, a12, a11, a10, a9, a8, a7, a6, a5, a4, a3, a2, a1)
  val nest14 = 
   fn (a14, a13, a12, a11, a10, a9, a8, a7, a6, a5, a4, a3, a2, a1) =>
      (a14, (a13, (a12, (a11, (a10, (a9, (a8, (a7, (a6, (a5, (a4, (a3, (a2, a1)))))))))))))

  val flatten15 = 
   fn (a15, (a14, (a13, (a12, (a11, (a10, (a9, (a8, (a7, (a6, (a5, (a4, (a3, (a2, a1)))))))))))))) =>
      (a15, a14, a13, a12, a11, a10, a9, a8, a7, a6, a5, a4, a3, a2, a1)
  val nest15 = 
   fn (a15, a14, a13, a12, a11, a10, a9, a8, a7, a6, a5, a4, a3, a2, a1) =>
      (a15, (a14, (a13, (a12, (a11, (a10, (a9, (a8, (a7, (a6, (a5, (a4, (a3, (a2, a1))))))))))))))

  val flatten16 = 
   fn (a16, (a15, (a14, (a13, (a12, (a11, (a10, (a9, (a8, (a7, (a6, (a5, (a4, (a3, (a2, a1))))))))))))))) =>
      (a16, a15, a14, a13, a12, a11, a10, a9, a8, a7, a6, a5, a4, a3, a2, a1)
  val nest16 = 
   fn (a16, a15, a14, a13, a12, a11, a10, a9, a8, a7, a6, a5, a4, a3, a2, a1) =>
      (a16, (a15, (a14, (a13, (a12, (a11, (a10, (a9, (a8, (a7, (a6, (a5, (a4, (a3, (a2, a1)))))))))))))))

  val flatten17 = 
   fn (a17, (a16, (a15, (a14, (a13, (a12, (a11, (a10, (a9, (a8, (a7, (a6, (a5, (a4, (a3, (a2, a1)))))))))))))))) =>
      (a17, a16, a15, a14, a13, a12, a11, a10, a9, a8, a7, a6, a5, a4, a3, a2, a1)
  val nest17 = 
   fn (a17, a16, a15, a14, a13, a12, a11, a10, a9, a8, a7, a6, a5, a4, a3, a2, a1) =>
      (a17, (a16, (a15, (a14, (a13, (a12, (a11, (a10, (a9, (a8, (a7, (a6, (a5, (a4, (a3, (a2, a1))))))))))))))))

  val flatten18 = 
   fn (a18, (a17, (a16, (a15, (a14, (a13, (a12, (a11, (a10, (a9, (a8, (a7, (a6, (a5, (a4, (a3, 
      (a2, a1))))))))))))))))) =>
      (a18, a17, a16, a15, a14, a13, a12, a11, a10, a9, a8, a7, a6, a5, a4, a3, a2, a1)
  val nest18 = 
   fn (a18, a17, a16, a15, a14, a13, a12, a11, a10, a9, a8, a7, a6, a5, a4, a3, a2, a1) =>
      (a18, (a17, (a16, (a15, (a14, (a13, (a12, (a11, (a10, (a9, (a8, (a7, (a6, (a5, (a4, (a3, (a2, a1)))))))))))))))))

  val flatten19 = 
   fn (a19, (a18, (a17, (a16, (a15, (a14, (a13, (a12, (a11, (a10, (a9, (a8, (a7, (a6, (a5, (a4, (a3, 
      (a2, a1)))))))))))))))))) =>
      (a19, a18, a17, a16, a15, a14, a13, a12, a11, a10, a9, a8, a7, a6, a5, a4, a3, a2, a1)
  val nest19 = 
   fn (a19, a18, a17, a16, a15, a14, a13, a12, a11, a10, a9, a8, a7, a6, a5, a4, a3, a2, a1) =>
      (a19, (a18, (a17, (a16, (a15, (a14, (a13, (a12, (a11, (a10, (a9, (a8, (a7, (a6, (a5, (a4, (a3, 
      (a2, a1))))))))))))))))))

  val flatten20 = 
   fn (a20, (a19, (a18, (a17, (a16, (a15, (a14, (a13, (a12, (a11, (a10, (a9, (a8, (a7, (a6, (a5, (a4, (a3, 
      (a2, a1))))))))))))))))))) =>
      (a20, a19, a18, a17, a16, a15, a14, a13, a12, a11, a10, a9, a8, a7, a6, a5, a4, a3, a2, a1)
  val nest20 = 
   fn (a20, a19, a18, a17, a16, a15, a14, a13, a12, a11, a10, a9, a8, a7, a6, a5, a4, a3, a2, a1) =>
      (a20, (a19, (a18, (a17, (a16, (a15, (a14, (a13, (a12, (a11, (a10, (a9, (a8, (a7, (a6, (a5, (a4, (a3, 
      (a2, a1)))))))))))))))))))

  val flatten21 = 
   fn (a21, (a20, (a19, (a18, (a17, (a16, (a15, (a14, (a13, (a12, (a11, (a10, (a9, (a8, (a7, (a6, (a5, (a4, (a3, 
      (a2, a1)))))))))))))))))))) =>
      (a21, a20, a19, a18, a17, a16, a15, a14, a13, a12, a11, a10, a9, a8, a7, a6, a5, a4, a3, a2, a1)
  val nest21 = 
   fn (a21, a20, a19, a18, a17, a16, a15, a14, a13, a12, a11, a10, a9, a8, a7, a6, a5, a4, a3, a2, a1) =>
      (a21, (a20, (a19, (a18, (a17, (a16, (a15, (a14, (a13, (a12, (a11, (a10, (a9, (a8, (a7, (a6, (a5, (a4, (a3, 
      (a2, a1))))))))))))))))))))

  val flatten22 = 
   fn (a22, (a21, (a20, (a19, (a18, (a17, (a16, (a15, (a14, (a13, (a12, (a11, (a10, (a9, (a8, (a7, (a6, (a5, (a4, (a3, 
      (a2, a1))))))))))))))))))))) =>
      (a22, a21, a20, a19, a18, a17, a16, a15, a14, a13, a12, a11, a10, a9, a8, a7, a6, a5, a4, a3, a2, a1)
  val nest22 = 
   fn (a22, a21, a20, a19, a18, a17, a16, a15, a14, a13, a12, a11, a10, a9, a8, a7, a6, a5, a4, a3, a2, a1) =>
      (a22, (a21, (a20, (a19, (a18, (a17, (a16, (a15, (a14, (a13, (a12, (a11, (a10, (a9, (a8, (a7, (a6, (a5, (a4, (a3, 
      (a2, a1)))))))))))))))))))))

  val flatten23 = 
   fn (a23, (a22, (a21, (a20, (a19, (a18, (a17, (a16, (a15, (a14, (a13, (a12, (a11, (a10, (a9, (a8, (a7, (a6, (a5, (a4, 
      (a3, (a2, a1)))))))))))))))))))))) =>
      (a23, a22, a21, a20, a19, a18, a17, a16, a15, a14, a13, a12, a11, a10, a9, a8, a7, a6, a5, a4, a3, a2, a1)
  val nest23 = 
   fn (a23, a22, a21, a20, a19, a18, a17, a16, a15, a14, a13, a12, a11, a10, a9, a8, a7, a6, a5, a4, a3, a2, a1) =>
      (a23, (a22, (a21, (a20, (a19, (a18, (a17, (a16, (a15, (a14, (a13, (a12, (a11, (a10, (a9, (a8, (a7, (a6, (a5, (a4, 
      (a3, (a2, a1))))))))))))))))))))))

  val flatten24 = 
   fn (a24, (a23, (a22, (a21, (a20, (a19, (a18, (a17, (a16, (a15, (a14, (a13, (a12, (a11, (a10, (a9, (a8, (a7, (a6, 
      (a5, (a4, (a3, (a2, a1))))))))))))))))))))))) =>
      (a24, a23, a22, a21, a20, a19, a18, a17, a16, a15, a14, a13, a12, a11, a10, a9, a8, a7, a6, a5, a4, a3, a2, a1)
  val nest24 = 
   fn (a24, a23, a22, a21, a20, a19, a18, a17, a16, a15, a14, a13, a12, a11, a10, a9, a8, a7, a6, a5, a4, a3, a2, a1) =>
      (a24, (a23, (a22, (a21, (a20, (a19, (a18, (a17, (a16, (a15, (a14, (a13, (a12, (a11, (a10, (a9, (a8, (a7, (a6, 
      (a5, (a4, (a3, (a2, a1)))))))))))))))))))))))

  val flatten25 = 
   fn (a25, (a24, (a23, (a22, (a21, (a20, (a19, (a18, (a17, (a16, (a15, (a14, (a13, (a12, (a11, (a10, (a9, (a8, (a7, 
      (a6, (a5, (a4, (a3, (a2, a1)))))))))))))))))))))))) =>
      (a25, a24, a23, a22, a21, a20, a19, a18, a17, a16, a15, a14, a13, a12, a11, a10, a9, a8, a7, a6, a5, a4, a3, a2, 
       a1)
  val nest25 = 
   fn (a25, a24, a23, a22, a21, a20, a19, a18, a17, a16, a15, a14, a13, a12, a11, a10, a9, a8, a7, a6, a5, a4, a3, a2, 
       a1) =>
      (a25, (a24, (a23, (a22, (a21, (a20, (a19, (a18, (a17, (a16, (a15, (a14, (a13, (a12, (a11, (a10, (a9, (a8, (a7, 
      (a6, (a5, (a4, (a3, (a2, a1))))))))))))))))))))))))

  val flatten26 = 
   fn (a26, (a25, (a24, (a23, (a22, (a21, (a20, (a19, (a18, (a17, (a16, (a15, (a14, (a13, (a12, (a11, (a10, (a9, (a8, 
      (a7, (a6, (a5, (a4, (a3, (a2, a1))))))))))))))))))))))))) =>
      (a26, a25, a24, a23, a22, a21, a20, a19, a18, a17, a16, a15, a14, a13, a12, a11, a10, a9, a8, a7, a6, a5, a4, a3, 
       a2, a1)
  val nest26 = 
   fn (a26, a25, a24, a23, a22, a21, a20, a19, a18, a17, a16, a15, a14, a13, a12, a11, a10, a9, a8, a7, a6, a5, a4, a3, 
       a2, a1) =>
      (a26, (a25, (a24, (a23, (a22, (a21, (a20, (a19, (a18, (a17, (a16, (a15, (a14, (a13, (a12, (a11, (a10, (a9, (a8, 
      (a7, (a6, (a5, (a4, (a3, (a2, a1)))))))))))))))))))))))))

  val flatten27 = 
   fn (a27, (a26, (a25, (a24, (a23, (a22, (a21, (a20, (a19, (a18, (a17, (a16, (a15, (a14, (a13, (a12, (a11, (a10, (a9, 
      (a8, (a7, (a6, (a5, (a4, (a3, (a2, a1)))))))))))))))))))))))))) =>
      (a27, a26, a25, a24, a23, a22, a21, a20, a19, a18, a17, a16, a15, a14, a13, a12, a11, a10, a9, a8, a7, a6, a5, 
       a4, a3, a2, a1)
  val nest27 = 
   fn (a27, a26, a25, a24, a23, a22, a21, a20, a19, a18, a17, a16, a15, a14, a13, a12, a11, a10, a9, a8, a7, a6, a5, 
       a4, a3, a2, a1) =>
      (a27, (a26, (a25, (a24, (a23, (a22, (a21, (a20, (a19, (a18, (a17, (a16, (a15, (a14, (a13, (a12, (a11, (a10, (a9, 
      (a8, (a7, (a6, (a5, (a4, (a3, (a2, a1))))))))))))))))))))))))))

  val flatten28 = 
   fn (a28, (a27, (a26, (a25, (a24, (a23, (a22, (a21, (a20, (a19, (a18, (a17, (a16, (a15, (a14, (a13, (a12, (a11, (a10, 
      (a9, (a8, (a7, (a6, (a5, (a4, (a3, (a2, a1))))))))))))))))))))))))))) =>
      (a28, a27, a26, a25, a24, a23, a22, a21, a20, a19, a18, a17, a16, a15, a14, a13, a12, a11, a10, a9, a8, a7, a6, 
       a5, a4, a3, a2, a1)
  val nest28 = 
   fn (a28, a27, a26, a25, a24, a23, a22, a21, a20, a19, a18, a17, a16, a15, a14, a13, a12, a11, a10, a9, a8, a7, a6, 
       a5, a4, a3, a2, a1) =>
      (a28, (a27, (a26, (a25, (a24, (a23, (a22, (a21, (a20, (a19, (a18, (a17, (a16, (a15, (a14, (a13, (a12, (a11, (a10, 
      (a9, (a8, (a7, (a6, (a5, (a4, (a3, (a2, a1)))))))))))))))))))))))))))

  val flatten29 = 
   fn (a29, (a28, (a27, (a26, (a25, (a24, (a23, (a22, (a21, (a20, (a19, (a18, (a17, (a16, (a15, (a14, (a13, (a12, (a11, 
      (a10, (a9, (a8, (a7, (a6, (a5, (a4, (a3, (a2, a1)))))))))))))))))))))))))))) =>
      (a29, a28, a27, a26, a25, a24, a23, a22, a21, a20, a19, a18, a17, a16, a15, a14, a13, a12, a11, a10, a9, a8, a7, 
       a6, a5, a4, a3, a2, a1)
  val nest29 = 
   fn (a29, a28, a27, a26, a25, a24, a23, a22, a21, a20, a19, a18, a17, a16, a15, a14, a13, a12, a11, a10, a9, a8, a7, 
       a6, a5, a4, a3, a2, a1) =>
      (a29, (a28, (a27, (a26, (a25, (a24, (a23, (a22, (a21, (a20, (a19, (a18, (a17, (a16, (a15, (a14, (a13, (a12, (a11, 
      (a10, (a9, (a8, (a7, (a6, (a5, (a4, (a3, (a2, a1))))))))))))))))))))))))))))

  val flatten30 = 
   fn (a30, (a29, (a28, (a27, (a26, (a25, (a24, (a23, (a22, (a21, (a20, (a19, (a18, (a17, (a16, (a15, (a14, (a13, (a12, 
      (a11, (a10, (a9, (a8, (a7, (a6, (a5, (a4, (a3, (a2, a1))))))))))))))))))))))))))))) =>
      (a30, a29, a28, a27, a26, a25, a24, a23, a22, a21, a20, a19, a18, a17, a16, a15, a14, a13, a12, a11, a10, a9, a8, 
       a7, a6, a5, a4, a3, a2, a1)
  val nest30 = 
   fn (a30, a29, a28, a27, a26, a25, a24, a23, a22, a21, a20, a19, a18, a17, a16, a15, a14, a13, a12, a11, a10, a9, a8, 
       a7, a6, a5, a4, a3, a2, a1) =>
      (a30, (a29, (a28, (a27, (a26, (a25, (a24, (a23, (a22, (a21, (a20, (a19, (a18, (a17, (a16, (a15, (a14, (a13, (a12, 
      (a11, (a10, (a9, (a8, (a7, (a6, (a5, (a4, (a3, (a2, a1)))))))))))))))))))))))))))))

end (* structure FlatTuple *)

(* Build functional get/set methods for a record type.  To use,
 * define functions mapping a record type into and out of a flat 
 * tuple type, and pass these to the mk function of the appropriate arity.  
 * The result is a flat tuple of (get, set) tuples, with (set, get) field 
 * of the tuple corresponding to the get/set method for the record
 * field mapped to the corresponding position by the record isomorphism.  
 * Example:
 *
 * datatype t = T of {a : int, b : bool, c : int}
 *
 * val ((setA, getA),
 *      (setB, getB),
 *      (setC, getC)) = 
 *    FunctionalUpdate.mk3 (fn (T{a, b, c}) => (a, b, c),
 *                          fn (a, b, c) => T{a = a, b = b, c = c})
 * *)
signature FUNCTIONAL_UPDATE =
sig
  type ('record, 'tuple) isomorphism = (* Mapping from record to tuple and back *)
                                         ('record -> 'tuple) * ('tuple -> 'record) 

  type ('record, 'elt) ops = (* A set/get method pair for a given record/elt type *)
                               ('record * 'elt -> 'record) * (* set *)
                               ('record -> 'elt)             (* get *)


  val mk1 : ('r, 'a1) isomorphism ->
            ('r, 'a1) ops
  val mk2 : ('r, 'a2 * 'a1) isomorphism ->
            ('r, 'a2) ops *
            ('r, 'a1) ops
  val mk3 : ('r, 'a3 * 'a2 * 'a1) isomorphism ->
            ('r, 'a3) ops *
            ('r, 'a2) ops *
            ('r, 'a1) ops
  val mk4 : ('r, 'a4 * 'a3 * 'a2 * 'a1) isomorphism ->
            ('r, 'a4) ops *
            ('r, 'a3) ops *
            ('r, 'a2) ops *
            ('r, 'a1) ops
  val mk5 : ('r, 'a5 * 'a4 * 'a3 * 'a2 * 'a1) isomorphism ->
            ('r, 'a5) ops *
            ('r, 'a4) ops *
            ('r, 'a3) ops *
            ('r, 'a2) ops *
            ('r, 'a1) ops
  val mk6 : ('r, 'a6 * 'a5 * 'a4 * 'a3 * 'a2 * 'a1) isomorphism ->
            ('r, 'a6) ops *
            ('r, 'a5) ops *
            ('r, 'a4) ops *
            ('r, 'a3) ops *
            ('r, 'a2) ops *
            ('r, 'a1) ops
  val mk7 : ('r, 'a7 * 'a6 * 'a5 * 'a4 * 'a3 * 'a2 * 'a1) isomorphism ->
            ('r, 'a7) ops *
            ('r, 'a6) ops *
            ('r, 'a5) ops *
            ('r, 'a4) ops *
            ('r, 'a3) ops *
            ('r, 'a2) ops *
            ('r, 'a1) ops
  val mk8 : ('r, 'a8 * 'a7 * 'a6 * 'a5 * 'a4 * 'a3 * 'a2 * 'a1) isomorphism ->
            ('r, 'a8) ops *
            ('r, 'a7) ops *
            ('r, 'a6) ops *
            ('r, 'a5) ops *
            ('r, 'a4) ops *
            ('r, 'a3) ops *
            ('r, 'a2) ops *
            ('r, 'a1) ops
  val mk9 : ('r, 'a9 * 'a8 * 'a7 * 'a6 * 'a5 * 'a4 * 'a3 * 'a2 * 'a1) isomorphism ->
            ('r, 'a9) ops *
            ('r, 'a8) ops *
            ('r, 'a7) ops *
            ('r, 'a6) ops *
            ('r, 'a5) ops *
            ('r, 'a4) ops *
            ('r, 'a3) ops *
            ('r, 'a2) ops *
            ('r, 'a1) ops
  val mk10 : ('r, 'a10 * 'a9 * 'a8 * 'a7 * 'a6 * 'a5 * 'a4 * 'a3 * 'a2 * 'a1) isomorphism ->
             ('r, 'a10) ops *
             ('r, 'a9) ops *
             ('r, 'a8) ops *
             ('r, 'a7) ops *
             ('r, 'a6) ops *
             ('r, 'a5) ops *
             ('r, 'a4) ops *
             ('r, 'a3) ops *
             ('r, 'a2) ops *
             ('r, 'a1) ops
  val mk11 : ('r, 'a11 * 'a10 * 'a9 * 'a8 * 'a7 * 'a6 * 'a5 * 'a4 * 'a3 * 'a2 * 'a1) isomorphism ->
             ('r, 'a11) ops *
             ('r, 'a10) ops *
             ('r, 'a9) ops *
             ('r, 'a8) ops *
             ('r, 'a7) ops *
             ('r, 'a6) ops *
             ('r, 'a5) ops *
             ('r, 'a4) ops *
             ('r, 'a3) ops *
             ('r, 'a2) ops *
             ('r, 'a1) ops
  val mk12 : ('r, 'a12 * 'a11 * 'a10 * 'a9 * 'a8 * 'a7 * 'a6 * 'a5 * 'a4 * 'a3 * 'a2 * 'a1) isomorphism ->
             ('r, 'a12) ops *
             ('r, 'a11) ops *
             ('r, 'a10) ops *
             ('r, 'a9) ops *
             ('r, 'a8) ops *
             ('r, 'a7) ops *
             ('r, 'a6) ops *
             ('r, 'a5) ops *
             ('r, 'a4) ops *
             ('r, 'a3) ops *
             ('r, 'a2) ops *
             ('r, 'a1) ops
  val mk13 : ('r, 'a13 * 'a12 * 'a11 * 'a10 * 'a9 * 'a8 * 'a7 * 'a6 * 'a5 * 'a4 * 'a3 * 'a2 * 'a1) isomorphism ->
             ('r, 'a13) ops *
             ('r, 'a12) ops *
             ('r, 'a11) ops *
             ('r, 'a10) ops *
             ('r, 'a9) ops *
             ('r, 'a8) ops *
             ('r, 'a7) ops *
             ('r, 'a6) ops *
             ('r, 'a5) ops *
             ('r, 'a4) ops *
             ('r, 'a3) ops *
             ('r, 'a2) ops *
             ('r, 'a1) ops
  val mk14 : ('r, 'a14 * 'a13 * 'a12 * 'a11 * 'a10 * 'a9 * 'a8 * 'a7 * 'a6 * 'a5 * 'a4 * 'a3 * 'a2 * 'a1) isomorphism ->
             ('r, 'a14) ops *
             ('r, 'a13) ops *
             ('r, 'a12) ops *
             ('r, 'a11) ops *
             ('r, 'a10) ops *
             ('r, 'a9) ops *
             ('r, 'a8) ops *
             ('r, 'a7) ops *
             ('r, 'a6) ops *
             ('r, 'a5) ops *
             ('r, 'a4) ops *
             ('r, 'a3) ops *
             ('r, 'a2) ops *
             ('r, 'a1) ops
  val mk15 : ('r, 'a15 * 'a14 * 'a13 * 'a12 * 'a11 * 'a10 * 'a9 * 'a8 * 'a7 * 'a6 * 'a5 * 'a4 * 'a3 * 'a2 
                       * 'a1) isomorphism ->
             ('r, 'a15) ops *
             ('r, 'a14) ops *
             ('r, 'a13) ops *
             ('r, 'a12) ops *
             ('r, 'a11) ops *
             ('r, 'a10) ops *
             ('r, 'a9) ops *
             ('r, 'a8) ops *
             ('r, 'a7) ops *
             ('r, 'a6) ops *
             ('r, 'a5) ops *
             ('r, 'a4) ops *
             ('r, 'a3) ops *
             ('r, 'a2) ops *
             ('r, 'a1) ops
  val mk16 : ('r, 'a16 * 'a15 * 'a14 * 'a13 * 'a12 * 'a11 * 'a10 * 'a9 * 'a8 * 'a7 * 'a6 * 'a5 * 'a4 * 'a3 * 'a2 
                       * 'a1) isomorphism ->
             ('r, 'a16) ops *
             ('r, 'a15) ops *
             ('r, 'a14) ops *
             ('r, 'a13) ops *
             ('r, 'a12) ops *
             ('r, 'a11) ops *
             ('r, 'a10) ops *
             ('r, 'a9) ops *
             ('r, 'a8) ops *
             ('r, 'a7) ops *
             ('r, 'a6) ops *
             ('r, 'a5) ops *
             ('r, 'a4) ops *
             ('r, 'a3) ops *
             ('r, 'a2) ops *
             ('r, 'a1) ops
  val mk17 : ('r, 'a17 * 'a16 * 'a15 * 'a14 * 'a13 * 'a12 * 'a11 * 'a10 * 'a9 * 'a8 * 'a7 * 'a6 * 'a5 * 'a4 * 'a3 
                       * 'a2 * 'a1) isomorphism ->
             ('r, 'a17) ops *
             ('r, 'a16) ops *
             ('r, 'a15) ops *
             ('r, 'a14) ops *
             ('r, 'a13) ops *
             ('r, 'a12) ops *
             ('r, 'a11) ops *
             ('r, 'a10) ops *
             ('r, 'a9) ops *
             ('r, 'a8) ops *
             ('r, 'a7) ops *
             ('r, 'a6) ops *
             ('r, 'a5) ops *
             ('r, 'a4) ops *
             ('r, 'a3) ops *
             ('r, 'a2) ops *
             ('r, 'a1) ops
  val mk18 : ('r, 'a18 * 'a17 * 'a16 * 'a15 * 'a14 * 'a13 * 'a12 * 'a11 * 'a10 * 'a9 * 'a8 * 'a7 * 'a6 * 'a5 * 'a4 
                       * 'a3 * 'a2 * 'a1) isomorphism ->
             ('r, 'a18) ops *
             ('r, 'a17) ops *
             ('r, 'a16) ops *
             ('r, 'a15) ops *
             ('r, 'a14) ops *
             ('r, 'a13) ops *
             ('r, 'a12) ops *
             ('r, 'a11) ops *
             ('r, 'a10) ops *
             ('r, 'a9) ops *
             ('r, 'a8) ops *
             ('r, 'a7) ops *
             ('r, 'a6) ops *
             ('r, 'a5) ops *
             ('r, 'a4) ops *
             ('r, 'a3) ops *
             ('r, 'a2) ops *
             ('r, 'a1) ops
  val mk19 : ('r, 'a19 * 'a18 * 'a17 * 'a16 * 'a15 * 'a14 * 'a13 * 'a12 * 'a11 * 'a10 * 'a9 * 'a8 * 'a7 * 'a6 * 'a5 
                       * 'a4 * 'a3 * 'a2 * 'a1) isomorphism ->
             ('r, 'a19) ops *
             ('r, 'a18) ops *
             ('r, 'a17) ops *
             ('r, 'a16) ops *
             ('r, 'a15) ops *
             ('r, 'a14) ops *
             ('r, 'a13) ops *
             ('r, 'a12) ops *
             ('r, 'a11) ops *
             ('r, 'a10) ops *
             ('r, 'a9) ops *
             ('r, 'a8) ops *
             ('r, 'a7) ops *
             ('r, 'a6) ops *
             ('r, 'a5) ops *
             ('r, 'a4) ops *
             ('r, 'a3) ops *
             ('r, 'a2) ops *
             ('r, 'a1) ops
  val mk20 : ('r, 'a20 * 'a19 * 'a18 * 'a17 * 'a16 * 'a15 * 'a14 * 'a13 * 'a12 * 'a11 * 'a10 * 'a9 * 'a8 * 'a7 * 'a6 
                       * 'a5 * 'a4 * 'a3 * 'a2 * 'a1) isomorphism ->
             ('r, 'a20) ops *
             ('r, 'a19) ops *
             ('r, 'a18) ops *
             ('r, 'a17) ops *
             ('r, 'a16) ops *
             ('r, 'a15) ops *
             ('r, 'a14) ops *
             ('r, 'a13) ops *
             ('r, 'a12) ops *
             ('r, 'a11) ops *
             ('r, 'a10) ops *
             ('r, 'a9) ops *
             ('r, 'a8) ops *
             ('r, 'a7) ops *
             ('r, 'a6) ops *
             ('r, 'a5) ops *
             ('r, 'a4) ops *
             ('r, 'a3) ops *
             ('r, 'a2) ops *
             ('r, 'a1) ops
  val mk21 : ('r, 'a21 * 'a20 * 'a19 * 'a18 * 'a17 * 'a16 * 'a15 * 'a14 * 'a13 * 'a12 * 'a11 * 'a10 * 'a9 * 'a8 * 'a7 
                       * 'a6 * 'a5 * 'a4 * 'a3 * 'a2 * 'a1) isomorphism ->
             ('r, 'a21) ops *
             ('r, 'a20) ops *
             ('r, 'a19) ops *
             ('r, 'a18) ops *
             ('r, 'a17) ops *
             ('r, 'a16) ops *
             ('r, 'a15) ops *
             ('r, 'a14) ops *
             ('r, 'a13) ops *
             ('r, 'a12) ops *
             ('r, 'a11) ops *
             ('r, 'a10) ops *
             ('r, 'a9) ops *
             ('r, 'a8) ops *
             ('r, 'a7) ops *
             ('r, 'a6) ops *
             ('r, 'a5) ops *
             ('r, 'a4) ops *
             ('r, 'a3) ops *
             ('r, 'a2) ops *
             ('r, 'a1) ops
  val mk22 : ('r, 'a22 * 'a21 * 'a20 * 'a19 * 'a18 * 'a17 * 'a16 * 'a15 * 'a14 * 'a13 * 'a12 * 'a11 * 'a10 * 'a9 * 'a8 
                       * 'a7 * 'a6 * 'a5 * 'a4 * 'a3 * 'a2 * 'a1) isomorphism ->
             ('r, 'a22) ops *
             ('r, 'a21) ops *
             ('r, 'a20) ops *
             ('r, 'a19) ops *
             ('r, 'a18) ops *
             ('r, 'a17) ops *
             ('r, 'a16) ops *
             ('r, 'a15) ops *
             ('r, 'a14) ops *
             ('r, 'a13) ops *
             ('r, 'a12) ops *
             ('r, 'a11) ops *
             ('r, 'a10) ops *
             ('r, 'a9) ops *
             ('r, 'a8) ops *
             ('r, 'a7) ops *
             ('r, 'a6) ops *
             ('r, 'a5) ops *
             ('r, 'a4) ops *
             ('r, 'a3) ops *
             ('r, 'a2) ops *
             ('r, 'a1) ops
  val mk23 : ('r, 'a23 * 'a22 * 'a21 * 'a20 * 'a19 * 'a18 * 'a17 * 'a16 * 'a15 * 'a14 * 'a13 * 'a12 * 'a11 * 'a10 
                       * 'a9 * 'a8 * 'a7 * 'a6 * 'a5 * 'a4 * 'a3 * 'a2 * 'a1) isomorphism ->
             ('r, 'a23) ops *
             ('r, 'a22) ops *
             ('r, 'a21) ops *
             ('r, 'a20) ops *
             ('r, 'a19) ops *
             ('r, 'a18) ops *
             ('r, 'a17) ops *
             ('r, 'a16) ops *
             ('r, 'a15) ops *
             ('r, 'a14) ops *
             ('r, 'a13) ops *
             ('r, 'a12) ops *
             ('r, 'a11) ops *
             ('r, 'a10) ops *
             ('r, 'a9) ops *
             ('r, 'a8) ops *
             ('r, 'a7) ops *
             ('r, 'a6) ops *
             ('r, 'a5) ops *
             ('r, 'a4) ops *
             ('r, 'a3) ops *
             ('r, 'a2) ops *
             ('r, 'a1) ops
  val mk24 : ('r, 'a24 * 'a23 * 'a22 * 'a21 * 'a20 * 'a19 * 'a18 * 'a17 * 'a16 * 'a15 * 'a14 * 'a13 * 'a12 * 'a11 
                       * 'a10 * 'a9 * 'a8 * 'a7 * 'a6 * 'a5 * 'a4 * 'a3 * 'a2 * 'a1) isomorphism ->
             ('r, 'a24) ops *
             ('r, 'a23) ops *
             ('r, 'a22) ops *
             ('r, 'a21) ops *
             ('r, 'a20) ops *
             ('r, 'a19) ops *
             ('r, 'a18) ops *
             ('r, 'a17) ops *
             ('r, 'a16) ops *
             ('r, 'a15) ops *
             ('r, 'a14) ops *
             ('r, 'a13) ops *
             ('r, 'a12) ops *
             ('r, 'a11) ops *
             ('r, 'a10) ops *
             ('r, 'a9) ops *
             ('r, 'a8) ops *
             ('r, 'a7) ops *
             ('r, 'a6) ops *
             ('r, 'a5) ops *
             ('r, 'a4) ops *
             ('r, 'a3) ops *
             ('r, 'a2) ops *
             ('r, 'a1) ops
  val mk25 : ('r, 'a25 * 'a24 * 'a23 * 'a22 * 'a21 * 'a20 * 'a19 * 'a18 * 'a17 * 'a16 * 'a15 * 'a14 * 'a13 * 'a12 
                       * 'a11 * 'a10 * 'a9 * 'a8 * 'a7 * 'a6 * 'a5 * 'a4 * 'a3 * 'a2 * 'a1) isomorphism ->
             ('r, 'a25) ops *
             ('r, 'a24) ops *
             ('r, 'a23) ops *
             ('r, 'a22) ops *
             ('r, 'a21) ops *
             ('r, 'a20) ops *
             ('r, 'a19) ops *
             ('r, 'a18) ops *
             ('r, 'a17) ops *
             ('r, 'a16) ops *
             ('r, 'a15) ops *
             ('r, 'a14) ops *
             ('r, 'a13) ops *
             ('r, 'a12) ops *
             ('r, 'a11) ops *
             ('r, 'a10) ops *
             ('r, 'a9) ops *
             ('r, 'a8) ops *
             ('r, 'a7) ops *
             ('r, 'a6) ops *
             ('r, 'a5) ops *
             ('r, 'a4) ops *
             ('r, 'a3) ops *
             ('r, 'a2) ops *
             ('r, 'a1) ops
  val mk26 : ('r, 'a26 * 'a25 * 'a24 * 'a23 * 'a22 * 'a21 * 'a20 * 'a19 * 'a18 * 'a17 * 'a16 * 'a15 * 'a14 * 'a13 
                       * 'a12 * 'a11 * 'a10 * 'a9 * 'a8 * 'a7 * 'a6 * 'a5 * 'a4 * 'a3 * 'a2 * 'a1) isomorphism ->
             ('r, 'a26) ops *
             ('r, 'a25) ops *
             ('r, 'a24) ops *
             ('r, 'a23) ops *
             ('r, 'a22) ops *
             ('r, 'a21) ops *
             ('r, 'a20) ops *
             ('r, 'a19) ops *
             ('r, 'a18) ops *
             ('r, 'a17) ops *
             ('r, 'a16) ops *
             ('r, 'a15) ops *
             ('r, 'a14) ops *
             ('r, 'a13) ops *
             ('r, 'a12) ops *
             ('r, 'a11) ops *
             ('r, 'a10) ops *
             ('r, 'a9) ops *
             ('r, 'a8) ops *
             ('r, 'a7) ops *
             ('r, 'a6) ops *
             ('r, 'a5) ops *
             ('r, 'a4) ops *
             ('r, 'a3) ops *
             ('r, 'a2) ops *
             ('r, 'a1) ops
  val mk27 : ('r, 'a27 * 'a26 * 'a25 * 'a24 * 'a23 * 'a22 * 'a21 * 'a20 * 'a19 * 'a18 * 'a17 * 'a16 * 'a15 * 'a14 
                       * 'a13 * 'a12 * 'a11 * 'a10 * 'a9 * 'a8 * 'a7 * 'a6 * 'a5 * 'a4 * 'a3 * 'a2 * 'a1) isomorphism ->
             ('r, 'a27) ops *
             ('r, 'a26) ops *
             ('r, 'a25) ops *
             ('r, 'a24) ops *
             ('r, 'a23) ops *
             ('r, 'a22) ops *
             ('r, 'a21) ops *
             ('r, 'a20) ops *
             ('r, 'a19) ops *
             ('r, 'a18) ops *
             ('r, 'a17) ops *
             ('r, 'a16) ops *
             ('r, 'a15) ops *
             ('r, 'a14) ops *
             ('r, 'a13) ops *
             ('r, 'a12) ops *
             ('r, 'a11) ops *
             ('r, 'a10) ops *
             ('r, 'a9) ops *
             ('r, 'a8) ops *
             ('r, 'a7) ops *
             ('r, 'a6) ops *
             ('r, 'a5) ops *
             ('r, 'a4) ops *
             ('r, 'a3) ops *
             ('r, 'a2) ops *
             ('r, 'a1) ops
  val mk28 : ('r, 'a28 * 'a27 * 'a26 * 'a25 * 'a24 * 'a23 * 'a22 * 'a21 * 'a20 * 'a19 * 'a18 * 'a17 * 'a16 * 'a15 
                       * 'a14 * 'a13 * 'a12 * 'a11 * 'a10 * 'a9 * 'a8 * 'a7 * 'a6 * 'a5 * 'a4 * 'a3 * 'a2 
                       * 'a1) isomorphism ->
             ('r, 'a28) ops *
             ('r, 'a27) ops *
             ('r, 'a26) ops *
             ('r, 'a25) ops *
             ('r, 'a24) ops *
             ('r, 'a23) ops *
             ('r, 'a22) ops *
             ('r, 'a21) ops *
             ('r, 'a20) ops *
             ('r, 'a19) ops *
             ('r, 'a18) ops *
             ('r, 'a17) ops *
             ('r, 'a16) ops *
             ('r, 'a15) ops *
             ('r, 'a14) ops *
             ('r, 'a13) ops *
             ('r, 'a12) ops *
             ('r, 'a11) ops *
             ('r, 'a10) ops *
             ('r, 'a9) ops *
             ('r, 'a8) ops *
             ('r, 'a7) ops *
             ('r, 'a6) ops *
             ('r, 'a5) ops *
             ('r, 'a4) ops *
             ('r, 'a3) ops *
             ('r, 'a2) ops *
             ('r, 'a1) ops
  val mk29 : ('r, 'a29 * 'a28 * 'a27 * 'a26 * 'a25 * 'a24 * 'a23 * 'a22 * 'a21 * 'a20 * 'a19 * 'a18 * 'a17 * 'a16 
                       * 'a15 * 'a14 * 'a13 * 'a12 * 'a11 * 'a10 * 'a9 * 'a8 * 'a7 * 'a6 * 'a5 * 'a4 * 'a3 
                       * 'a2 * 'a1) isomorphism ->
             ('r, 'a29) ops *
             ('r, 'a28) ops *
             ('r, 'a27) ops *
             ('r, 'a26) ops *
             ('r, 'a25) ops *
             ('r, 'a24) ops *
             ('r, 'a23) ops *
             ('r, 'a22) ops *
             ('r, 'a21) ops *
             ('r, 'a20) ops *
             ('r, 'a19) ops *
             ('r, 'a18) ops *
             ('r, 'a17) ops *
             ('r, 'a16) ops *
             ('r, 'a15) ops *
             ('r, 'a14) ops *
             ('r, 'a13) ops *
             ('r, 'a12) ops *
             ('r, 'a11) ops *
             ('r, 'a10) ops *
             ('r, 'a9) ops *
             ('r, 'a8) ops *
             ('r, 'a7) ops *
             ('r, 'a6) ops *
             ('r, 'a5) ops *
             ('r, 'a4) ops *
             ('r, 'a3) ops *
             ('r, 'a2) ops *
             ('r, 'a1) ops
  val mk30 : ('r, 'a30 * 'a29 * 'a28 * 'a27 * 'a26 * 'a25 * 'a24 * 'a23 * 'a22 * 'a21 * 'a20 * 'a19 * 'a18 * 'a17 
                       * 'a16 * 'a15 * 'a14 * 'a13 * 'a12 * 'a11 * 'a10 * 'a9 * 'a8 * 'a7 * 'a6 * 'a5 * 'a4 
                       * 'a3 * 'a2 * 'a1) isomorphism ->
             ('r, 'a30) ops *
             ('r, 'a29) ops *
             ('r, 'a28) ops *
             ('r, 'a27) ops *
             ('r, 'a26) ops *
             ('r, 'a25) ops *
             ('r, 'a24) ops *
             ('r, 'a23) ops *
             ('r, 'a22) ops *
             ('r, 'a21) ops *
             ('r, 'a20) ops *
             ('r, 'a19) ops *
             ('r, 'a18) ops *
             ('r, 'a17) ops *
             ('r, 'a16) ops *
             ('r, 'a15) ops *
             ('r, 'a14) ops *
             ('r, 'a13) ops *
             ('r, 'a12) ops *
             ('r, 'a11) ops *
             ('r, 'a10) ops *
             ('r, 'a9) ops *
             ('r, 'a8) ops *
             ('r, 'a7) ops *
             ('r, 'a6) ops *
             ('r, 'a5) ops *
             ('r, 'a4) ops *
             ('r, 'a3) ops *
             ('r, 'a2) ops *
             ('r, 'a1) ops
end (* signature FUNCTIONAL_UPDATE *)

structure FunctionalUpdate :> FUNCTIONAL_UPDATE = 
struct


  type ('record, 'tuple) isomorphism = (* Mapping from record to tuple and back *)
       ('record -> 'tuple) * ('tuple -> 'record) 

  type ('record, 'elt) ops = (* A set/get method pair for a given record/elt type *)
       ('record * 'elt -> 'record) * (* set *)
       ('record -> 'elt)             (* get *)


  type ('record, 'elts, 'setters) mker = 
       ('record -> ('elts -> 'record) * 'elts)
       -> 'setters

  val mk1' : ('r, 'e, ('r, 'e) ops) mker = 
   fn split => 
      let
        val set1 = 
         fn (r, a) => 
            let
              val (t2r, _) = split r
            in t2r a
            end
        val get1 = #2 o split
      in (set1, get1)
      end


  val mker 
      : ('r,      'e,                  's) mker ->
        ('r, 'f * 'e, (('r, 'f) ops) * 's) mker = 
   fn mk => 
   fn split => 
      let
        val set1 = 
         fn (r, a) => 
            let
              val (t2r, (_, b)) = split r
            in t2r (a, b)
            end
        val get1 = #1 o #2 o split
        val split = 
         fn r => 
            let
              val (t2r, (a, b)) = split r
              val t2r = fn b => t2r (a, b)
            in (t2r, b)
            end
        val set2 = mk split
      in ((set1, get1), set2)
      end

  val mk2' = fn args => mker mk1' args
  val mk3' = fn args => mker mk2' args
  val mk4' = fn args => mker mk3' args
  val mk5' = fn args => mker mk4' args
  val mk6' = fn args => mker mk5' args
  val mk7' = fn args => mker mk6' args
  val mk8' = fn args => mker mk7' args
  val mk9' = fn args => mker mk8' args
  val mk10' = fn args => mker mk9' args
  val mk11' = fn args => mker mk10' args
  val mk12' = fn args => mker mk11' args
  val mk13' = fn args => mker mk12' args
  val mk14' = fn args => mker mk13' args
  val mk15' = fn args => mker mk14' args
  val mk16' = fn args => mker mk15' args
  val mk17' = fn args => mker mk16' args
  val mk18' = fn args => mker mk17' args
  val mk19' = fn args => mker mk18' args
  val mk20' = fn args => mker mk19' args
  val mk21' = fn args => mker mk20' args
  val mk22' = fn args => mker mk21' args
  val mk23' = fn args => mker mk22' args
  val mk24' = fn args => mker mk23' args
  val mk25' = fn args => mker mk24' args
  val mk26' = fn args => mker mk25' args
  val mk27' = fn args => mker mk26' args
  val mk28' = fn args => mker mk27' args
  val mk29' = fn args => mker mk28' args
  val mk30' = fn args => mker mk29' args

                         
  structure FT = FlatTuple

  val mk1 = fn (r2t, t2r) => FT.flatten1 (mk1' (fn a => (t2r o FT.flatten1, FT.nest1 (r2t a))))
  val mk2 = fn (r2t, t2r) => FT.flatten2 (mk2' (fn a => (t2r o FT.flatten2, FT.nest2 (r2t a))))
  val mk3 = fn (r2t, t2r) => FT.flatten3 (mk3' (fn a => (t2r o FT.flatten3, FT.nest3 (r2t a))))
  val mk4 = fn (r2t, t2r) => FT.flatten4 (mk4' (fn a => (t2r o FT.flatten4, FT.nest4 (r2t a))))
  val mk5 = fn (r2t, t2r) => FT.flatten5 (mk5' (fn a => (t2r o FT.flatten5, FT.nest5 (r2t a))))
  val mk6 = fn (r2t, t2r) => FT.flatten6 (mk6' (fn a => (t2r o FT.flatten6, FT.nest6 (r2t a))))
  val mk7 = fn (r2t, t2r) => FT.flatten7 (mk7' (fn a => (t2r o FT.flatten7, FT.nest7 (r2t a))))
  val mk8 = fn (r2t, t2r) => FT.flatten8 (mk8' (fn a => (t2r o FT.flatten8, FT.nest8 (r2t a))))
  val mk9 = fn (r2t, t2r) => FT.flatten9 (mk9' (fn a => (t2r o FT.flatten9, FT.nest9 (r2t a))))
  val mk10 = fn (r2t, t2r) => FT.flatten10 (mk10' (fn a => (t2r o FT.flatten10, FT.nest10 (r2t a))))
  val mk11 = fn (r2t, t2r) => FT.flatten11 (mk11' (fn a => (t2r o FT.flatten11, FT.nest11 (r2t a))))
  val mk12 = fn (r2t, t2r) => FT.flatten12 (mk12' (fn a => (t2r o FT.flatten12, FT.nest12 (r2t a))))
  val mk13 = fn (r2t, t2r) => FT.flatten13 (mk13' (fn a => (t2r o FT.flatten13, FT.nest13 (r2t a))))
  val mk14 = fn (r2t, t2r) => FT.flatten14 (mk14' (fn a => (t2r o FT.flatten14, FT.nest14 (r2t a))))
  val mk15 = fn (r2t, t2r) => FT.flatten15 (mk15' (fn a => (t2r o FT.flatten15, FT.nest15 (r2t a))))
  val mk16 = fn (r2t, t2r) => FT.flatten16 (mk16' (fn a => (t2r o FT.flatten16, FT.nest16 (r2t a))))
  val mk17 = fn (r2t, t2r) => FT.flatten17 (mk17' (fn a => (t2r o FT.flatten17, FT.nest17 (r2t a))))
  val mk18 = fn (r2t, t2r) => FT.flatten18 (mk18' (fn a => (t2r o FT.flatten18, FT.nest18 (r2t a))))
  val mk19 = fn (r2t, t2r) => FT.flatten19 (mk19' (fn a => (t2r o FT.flatten19, FT.nest19 (r2t a))))
  val mk20 = fn (r2t, t2r) => FT.flatten20 (mk20' (fn a => (t2r o FT.flatten20, FT.nest20 (r2t a))))
  val mk21 = fn (r2t, t2r) => FT.flatten21 (mk21' (fn a => (t2r o FT.flatten21, FT.nest21 (r2t a))))
  val mk22 = fn (r2t, t2r) => FT.flatten22 (mk22' (fn a => (t2r o FT.flatten22, FT.nest22 (r2t a))))
  val mk23 = fn (r2t, t2r) => FT.flatten23 (mk23' (fn a => (t2r o FT.flatten23, FT.nest23 (r2t a))))
  val mk24 = fn (r2t, t2r) => FT.flatten24 (mk24' (fn a => (t2r o FT.flatten24, FT.nest24 (r2t a))))
  val mk25 = fn (r2t, t2r) => FT.flatten25 (mk25' (fn a => (t2r o FT.flatten25, FT.nest25 (r2t a))))
  val mk26 = fn (r2t, t2r) => FT.flatten26 (mk26' (fn a => (t2r o FT.flatten26, FT.nest26 (r2t a))))
  val mk27 = fn (r2t, t2r) => FT.flatten27 (mk27' (fn a => (t2r o FT.flatten27, FT.nest27 (r2t a))))
  val mk28 = fn (r2t, t2r) => FT.flatten28 (mk28' (fn a => (t2r o FT.flatten28, FT.nest28 (r2t a))))
  val mk29 = fn (r2t, t2r) => FT.flatten29 (mk29' (fn a => (t2r o FT.flatten29, FT.nest29 (r2t a))))
  val mk30 = fn (r2t, t2r) => FT.flatten30 (mk30' (fn a => (t2r o FT.flatten30, FT.nest30 (r2t a))))

end (* structure FunctionalUpdate *)
