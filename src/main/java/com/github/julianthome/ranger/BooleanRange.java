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

public class BooleanRange extends AtomicNumRange {

    final static Logger LOGGER = LoggerFactory.getLogger(BooleanRange.class);

    public BooleanRange() {
        super(BooleanCut.TRUE, BooleanCut.FALSE);
    }

    public BooleanRange(NumCut min, NumCut max) {
        super(min,max);
    }

    public BooleanRange(BooleanRange br) {
        super(br.getMin(),br.getMax());
    }

    public BooleanRange(BooleanCut val) {
        super(val,val);
    }

    public BooleanRange or(BooleanRange other) {

        NumCut newmin = (other.getMin().add(getMin())).div(2L);
        NumCut newmax = (other.getMax().add(getMax())).div(2L);

        return new BooleanRange(newmin, newmax);
    }

    public BooleanRange and(BooleanRange other) {

        NumCut newmin = (other.getMin().add(getMin()).isGreaterThan(new
                NumCut(0L))) ?
                BooleanCut.FALSE.clone() : BooleanCut.TRUE.clone();
        NumCut newmax = (other.getMax().add(getMax()).isGreaterThan(new
                NumCut(0L))) ?
                BooleanCut.FALSE.clone() : BooleanCut.TRUE.clone();

        return new BooleanRange(newmin, newmax);
    }

    public BooleanRange xor(BooleanRange other) {

        BooleanRange pandq = this.and(other);
        BooleanRange qorq = this.or(other);

        LOGGER.debug("1" + pandq);
        LOGGER.debug("2" + qorq);

        BooleanRange neg = pandq.negate();
        LOGGER.debug("+1" + pandq);

        return qorq.and(neg);

    }

    public BooleanRange negate() {

        BooleanRange cp = this.clone();

        LOGGER.debug("NEGATE {} {}", cp.getMin(), cp.getMax());
        if(cp.isCatState()) {
            return cp;
        }

        if(cp.isAlwaysTrue()) {
            cp.setMax(BooleanCut.FALSE.clone());
            cp.setMin(BooleanCut.FALSE.clone());
        } else {
            cp.setMax(BooleanCut.TRUE.clone());
            cp.setMin(BooleanCut.TRUE.clone());
        }
        return cp;
    }

    public boolean isAlwaysTrue() {
        return (getMax().equals(BooleanCut.TRUE) && getMin().equals(getMax()));
    }

    public boolean isAlwaysFalse() {
        return (getMax().equals(BooleanCut.FALSE) && getMin().equals(getMax()));
    }

    public boolean isCatState() {
        return (getMin().equals(BooleanCut.TRUE) && getMax().equals(BooleanCut
                .FALSE));
    }

    @Override
    public BooleanRange intersect(Range dother) {

        BooleanRange other = (BooleanRange)dother;

        AtomicNumRange nr = super.intersect(other);

        if(nr == null) {
            return null;
        }

        BooleanRange br = new BooleanRange(nr.getMin(), nr.getMax());

        return br;
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("[");

        if(getMin().equals(BooleanCut.TRUE)) {
            sb.append("T");
        }

        if(getMax().equals(BooleanCut.FALSE)) {

            if(sb.length() > 1) {
                sb.append("|");
            }

            sb.append("F");
        }
        sb.append("]");
        return sb.toString();
    }

    @Override
    public BooleanRange clone() {
        return new BooleanRange(this);
    }

    @Override
    public boolean equals(Object o) {
        if(!(o instanceof BooleanRange))
            return false;

        BooleanRange ro = (BooleanRange)o;

        if(isAlwaysTrue() && ro.isAlwaysTrue())
            return true;

        if(isAlwaysFalse() && ro.isAlwaysFalse())
            return true;

        if(isCatState() && ro.isCatState())
            return true;

        return false;
    }

}
