# Sierpinski Triangle

A [sierpinski triangle][1] is a simple fractal.  This toy forth program
was written in response to [yet another "challenge"][2] on `comp.lang.forth`.

_Edit: 2015-05-18_: I did a version in scala while learning the language, `sierp.scala`,
                    and added an implementation in prolog for good measure. 

_Edit: 2015-06-09_: Added a c++ program, which came out more like the forth version
                    than I expected.

<pre><code>
                                *
                               * *
                              *   *
                             * * * *
                            *       *
                           * *     * *
                          *   *   *   *
                         * * * * * * * *
                        *               *
                       * *             * *
                      *   *           *   *
                     * * * *         * * * *
                    *       *       *       *
                   * *     * *     * *     * *
                  *   *   *   *   *   *   *   *
                 * * * * * * * * * * * * * * * *
                *                               *
               * *                             * *
              *   *                           *   *
             * * * *                         * * * *
            *       *                       *       *
           * *     * *                     * *     * *
          *   *   *   *                   *   *   *   *
         * * * * * * * *                 * * * * * * * *
        *               *               *               *
       * *             * *             * *             * *
      *   *           *   *           *   *           *   *
     * * * *         * * * *         * * * *         * * * *
    *       *       *       *       *       *       *       *
   * *     * *     * *     * *     * *     * *     * *     * *
  *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
</code></pre>

[1]: http://en.wikipedia.org/wiki/Sierpinski_triangle
[2]: https://groups.google.com/d/topic/comp.lang.forth/vJLNc8KfMTY/discussion
