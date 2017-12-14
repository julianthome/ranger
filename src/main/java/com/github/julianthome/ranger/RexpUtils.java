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
        return getRexpFor(true, min - 1);
    }

    public String getRexpForMaxInclusive(long max) {
        return getRexpFor(false, max + 1);
    }

    public String getRexpForMinExclusive(long min) {
        return getRexpFor(true, min);
    }


    public String getRexpForMaxExclusive(long max) {
        return getRexpFor(false, max);
    }


    private String getGroup(int min, int max) {
        if(min == max)
            return String.valueOf(min);

        return "[" + min + "-" + max +"]";
    }

    private void addOption(StringBuilder sb, String opt) {
        if(sb.length() > 0)
            sb.insert(0,"|");
        sb.insert(0,opt);
    }

    private String getRexpFor(boolean upwards, long number) {

        String sig = "";

        if (number < 0) {
            sig = "-";
            number *= -1;
            upwards = !upwards;
        } else if (number == 0) {
            return "(" + (upwards ? "" : "\\-") + NPFX + "*)";
        }

        assert (number > 0);

        String snum = Long.toString(number);

        StringBuilder drexp = new StringBuilder();

        int boundary = upwards ? 9 : 0;

        for (int l = snum.length() - 1; l >= 0; l--) {

            int digit = Integer.parseInt(String.valueOf(snum.charAt(l)));

            if (digit != boundary) {

                digit = upwards ? digit + 1 : digit - 1;

                String carry = getGroup(Math.min(digit, boundary), Math.max
                        (digit, boundary));

                addOption(drexp, sig
                        + snum.substring(0, l)
                        + carry
                        + StringUtils.repeat("[0-9]", snum.length() - l - 1));
            }
        }

        if (upwards) {
            addOption(drexp, sig + NPFX + "{" + (snum.length()) + ",}");
        } else {

            if (snum.length() > 1) {
                addOption(drexp, sig + NPFX + "{0," + (snum.length() - 2) + "}");
            }

            if (sig.equals(""))
                addOption(drexp,"0|\\-" + NPFX + "*");
            else
                addOption(drexp,"[0-9]|" + NPFX + "*");

        }

        return "(" + drexp.toString() + ")";
    }


}
