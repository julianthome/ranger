/*
 * cnetwork - a constraint network implementation for Java
 * Copyright (C) 2017 Julian Thome <julian.thome.de@gmail.com>
 *
 * cnetwork is licensed under the EUPL, Version 1.1 or – as soon
 * they will be approved by the European Commission - subsequent versions of the
 * EUPL (the "Licence"); You may not use this work except in compliance with the
 * Licence. You may obtain a copy of the Licence at:
 *
 * https://joinup.ec.europa.eu/sites/default/files/eupl1.1.-licence-en_0.pdf
 *
 * Unless required by applicable law or agreed to in writing, software distributed
 * under the Licence is distributed on an "AS IS" basis, WITHOUT WARRANTIES OR
 * CONDITIONS OF ANY KIND, either express or implied.  See the Licence for the
 * specific language governing permissions and limitations under the Licence.
 */

package com.github.julianthome.ranger;

import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


public enum RexpUtils {

    INSTANCE;

    public static String ALL = "(0|-?[1-9])[0-9]*";

    final Logger LOGGER = LoggerFactory.getLogger(RexpUtils.class);

    public String getRexpForRangeExclusive(long min, long max) {
        return getRexpForMinExclusive(min) + "&" + getRexpForMaxExclusive(max);
    }

    public String getRexpForRangeInclusive(long min, long max) {
        return getRexpForMinInclusive(min) + "&" + getRexpForMaxInclusive(max);
    }

    public String getRexpForMinInclusive(long min) {
        return getRexpForMinExclusive(min-1);
    }

    public String getRexpForMaxInclusive(long max) {
        return getRexpForMaxExclusive(max+1);
    }


    public String getRexpForMinExclusive(long min) {
        if (min < 0) {
            min *= -1;
            return "(" + getRexpForMax(min, "-") + "|[0-9]|[1-9][0-9]*)";
        } else if (min > 0) {
            // no prefix required
            return "(" + getRexpForMin(min, "") + ")";
        } else { // treat zero as a special case
            return "([1-9]|[1-9][0-9]+)";
        }
    }


    public String getRexpForMaxExclusive(long max) {
        if (max < 0) {
            max *= -1;
            return "(" + getRexpForMin(max, "-") + ")";
        } else if (max > 0) {
            // no prefix required
            return "(" + getRexpForMax(max, "") + "|0|-[1-9][0-9]*)";
        } else { // treat zero as a special case
            return "(-[1-9]|-[1-9][0-9]+)";
        }
    }


    private String getRexpForMin(long min, String pfx) {

        // The procedure below just works for non-negative integers
        assert (min > 0);

        String mins = Long.toString(min);
        char[] minc = mins.toCharArray();

        StringBuilder drexp = new StringBuilder();
        String option = "";

        for (int l = minc.length - 1; l >= 0; l--) {

            String carry = "";

            if (drexp.length() > 0) {
                option = "|";
            }


            char digit = minc[l];

            if (digit != '9') {

                if (l != minc.length) {
                    ++digit;

                    if (digit != '9') {
                        carry = "[" + (digit) + "-9]";
                    } else {
                        carry = "9";
                    }

                    drexp.insert(0, pfx +
                            mins.substring(0, l) +
                            carry + StringUtils.repeat("[0-9]", mins.length() - l - 1) + option);
                }
            }
        }

        // Meta rule for matching everything that has more digits
        if (drexp.length() > 0) {
            drexp.append("|");
        }
        drexp.append(pfx + "[1-9][0-9]{" + (mins.length()) + ",}");


        return drexp.toString();

    }


    private String getRexpForMax(long max, String pfx) {

        // The procedure below just works for non-negative integers
        assert (max > 0);

        String maxs = Long.toString(max);

        char[] maxc = maxs.toCharArray();

        StringBuilder drexp = new StringBuilder();

        String option = "";

        for (int l = maxc.length - 1; l >= 0; l--) {

            String carry = "";

            if (drexp.length() > 0) {
                option = "|";
            }

            char digit = maxc[l];


            if (digit != '0') {
                if (l != maxc.length) {
                    --digit;

                    if (digit != '0') {
                        carry = "[0-" + (digit) + "]";
                        //LOGGER.info("CARRY " + carry);
                    } else {
                        // if we reached the max significant digit
                        carry = "0";
                    }

                    String digits = maxs.substring(0, l);

                    drexp.insert(0, pfx +
                            digits +
                            carry + StringUtils.repeat("[0-9]", maxs.length() - l - 1) + option);
                }
            }


            if (maxs.length() > 1) {

                if (drexp.length() > 0) {
                    drexp.append("|");
                }

                drexp.append(pfx + "[1-9][0-9]{0," + (maxs.length() - 2) + "}");
            }

        }
        //LOGGER.info("DREXP " + drexp.toString());
        return drexp.toString();

    }


}
