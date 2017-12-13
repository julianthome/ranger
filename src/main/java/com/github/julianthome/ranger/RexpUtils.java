/**
 * ranger: a library for dealing with boolean and numeric (scattered) ranges
 * <p>
 * The MIT License (MIT)
 * <p>
 * Copyright (c) 2017 Julian Thome <julian.thome.de@gmail.com>
 * <p>
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
 * of the Software, and to permit persons to whom the Software is furnished to do
 * so, subject to the following conditions:
 * <p>
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 * <p>
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 **/

package com.github.julianthome.ranger;

import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


public enum RexpUtils {

    INSTANCE;

    public static String ALL = "(0|-?[1-9])[0-9]*";
    private static String NPFX = "[1-9][0-9]";

    final Logger LOGGER = LoggerFactory.getLogger(RexpUtils.class);

    public String getRexpForRangeExclusive(long min, long max) {
        return getRexpForMinExclusive(min) + "&" + getRexpForMaxExclusive(max);
    }

    public String getRexpForRangeInclusive(long min, long max) {
        return getRexpForMinInclusive(min) + "&" + getRexpForMaxInclusive(max);
    }

    public String getRexpForMinInclusive(long min) {
        return getRexpForMinExclusive(min - 1);
    }

    public String getRexpForMaxInclusive(long max) {
        return getRexpForMaxExclusive(max + 1);
    }


    public String getRexpForMinExclusive(long min) {
        if (min < 0) {
            min *= -1;
            return "(" + getRexpFor(false, min, "-") + "|[0-9]|" + NPFX + "*)";
        } else if (min > 0) {
            // no prefix required
            return "(" + getRexpFor(true, min, "") + ")";
        } else { // treat zero as a special case
            return NPFX + "*";
        }
    }


    public String getRexpForMaxExclusive(long max) {
        if (max < 0) {
            max *= -1;
            return "(" + getRexpFor(true, max, "-") + ")";
        } else if (max > 0) {
            // no prefix required
            return "(" + getRexpFor(false, max, "") + "|0|\\-" + NPFX + "*)";
        } else { // treat zero as a special case
            return "\\-" + NPFX + "*";
        }
    }


    private String getRexpFor(boolean upwards, long number, String sig) {

        // The procedure below just works for non-negative integers
        assert (number > 0);

        String snum = Long.toString(number);

        StringBuilder drexp = new StringBuilder();

        int boundary = upwards ? 9 : 0;

        for (int l = snum.length() - 1; l >= 0; l--) {

            int digit = Integer.parseInt(String.valueOf(snum.charAt(l)));

            if (digit != boundary) {

                digit = upwards ? digit + 1 : digit - 1;

                String carry = digit != boundary
                        ? "[" + Math.min(digit, boundary) + "-" + Math.max(digit, boundary) + "]"
                        : String.valueOf(boundary);

                drexp.insert(0, sig +
                        snum.substring(0, l) +
                        carry + StringUtils.repeat("[0-9]", snum.length() -
                        l - 1) + (drexp.length() > 0 ? "|" : ""));

            }
        }
        if (upwards) {
            if (drexp.length() > 0) {
                drexp.append("|");
            }
            drexp.append(sig + NPFX + "{" + (snum.length()) + ",}");
        } else if (snum.length() > 1) {
            if (drexp.length() > 0) {
                drexp.append("|");
            }
            drexp.append(sig + NPFX + "{0," + (snum.length() - 2) + "}");
        }

        return drexp.toString();

    }


}
