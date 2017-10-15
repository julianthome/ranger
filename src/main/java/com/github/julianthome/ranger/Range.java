/**
 * ranger: a library for dealing with boolean and numeric (scattered) ranges
 *
 * The MIT License (MIT)
 *
 * Copyright (c) 2017 Julian Thome <julian.thome.de@gmail.com>
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
 * of the Software, and to permit persons to whom the Software is furnished to do
 * so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 **/

package com.github.julianthome.ranger;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public abstract class Range {

    private final static Logger LOGGER = LoggerFactory.getLogger(Range.class);

    protected NumCut lb;
    protected NumCut ub;

    public Range(NumCut min, NumCut max) {

        //LOGGER.debug("new ran min:{} max:{}", min, max);
        assert min.isSmallerEqualsThan(max);
        this.lb = min;
        this.ub = max;
    }

    public Range() {
        lb = new BelowAll();
        ub = new AboveAll();
    }

    public NumCut getMin() {
        return lb;
    }

    public void setMin(NumCut min) {
        lb = min;
    }

    public void setMax(NumCut max) {
        ub = max;
    }


    public void setMin(long min) {
        lb = new NumCut(min);
    }

    public NumCut getMax() {
        return ub;
    }

    public void setMax(long max) {
        NumCut mx = new NumCut(max);
        this.lb.isSmallerEqualsThan(mx);
        this.ub = mx;
    }

    public abstract boolean contains( long value );

    public NumCut getDiff(){

        if(ub.isFixed() && lb.isFixed())
            new NumCut(ub.sub(lb));

        if(!ub.isFixed() || !lb.isFixed())
            return new AboveAll();

        assert false;
        return null;
    }

    public boolean isAlwaysGreaterThan(Range other){
        LOGGER.debug("lb {} > other.ub {}", lb, other.ub);

        return lb.isGreaterThan(other.ub);
    }

    public boolean isAlwaysSmallerThan(Range other){
        return ub.isSmallerThan(other.lb);
    }

    public boolean isBetween(long min, long max) {
        return lb.isGreaterEqualsThan(new NumCut(min)) &&
                ub.isSmallerEqualsThan(new NumCut(max));
    }

    public boolean isBetween(NumCut min, NumCut max) {
        return lb.isGreaterEqualsThan(min) &&
                ub.isSmallerEqualsThan(max);
    }

    public abstract Range clone();

    public String getDomainName() {
        return "range";
    }

    public abstract boolean equals(Object o);

    public abstract String toRegex();

}
