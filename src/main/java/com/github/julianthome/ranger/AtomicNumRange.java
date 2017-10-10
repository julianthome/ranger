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


public class AtomicNumRange extends Range {

    private final static Logger LOGGER = LoggerFactory.getLogger(AtomicNumRange.class);

    public static AtomicNumRange N = new AtomicNumRange(new NumCut(0L), new
            AboveAll());
    public static AtomicNumRange Z = new AtomicNumRange();
    public static AtomicNumRange E = new AtomicNumRange(new NumCut(0L), new
            NumCut(0L));


    public AtomicNumRange(long min, long max) {
        super(new NumCut(min),new NumCut(max));
    }

    public AtomicNumRange(NumCut min, NumCut max) {
        super(min,max);
    }

    public AtomicNumRange(long minmax) {
        this(minmax,minmax);
    }

    public AtomicNumRange() {
        super(new BelowAll(), new AboveAll());
    }

    public AtomicNumRange(AtomicNumRange nr) {
        this(nr.getMin().clone(),nr.getMax().clone());
    }


    public boolean isEmpty() {

        return false;
        //LOGGER.debug("this {}", toString());
        //return lb.equals(new NumCut(0L)) && ub.equals
        //        (new NumCut(0L));
    }

    public Range complement() {
        return Z.clone().minus(this);
    }

    @Override
    public int hashCode() {
        return getMin().hashCode();
    }

    @Override
    public String toString() {
        return "[" + this.getMin() + "," + this.getMax() + "]";
    }

    @Override
    public boolean contains( long value ) {

        return getMin().isSmallerEqualsThan(value) && getMax()
                .isGreaterEqualsThan(value);
    }

    public boolean subsumes(Range dother) {

        assert dother instanceof AtomicNumRange;

        AtomicNumRange o = (AtomicNumRange)dother;

        return getMin().isSmallerEqualsThan(o.getMin()) && getMax()
                .isGreaterEqualsThan(o.getMax());
    }

    public boolean isSingleton(){

        LOGGER.debug("is singleton {} {} {}", lb, ub, lb.equals(ub));
        return lb.equals(ub);
    }

    public Range union(Range dother) {

        assert dother instanceof AtomicNumRange;

        LOGGER.debug("unify {} - {}", this, dother);

        AtomicNumRange o = (AtomicNumRange)dother;

        if(intersect(o) == null) {
            LOGGER.debug("intersection is emtpy");
            if(getMax().add(1L).equals(o.getMin()) ||
                    getMin().sub(1L).equals(o.getMax())) {
                LOGGER.debug("new atomic range");
                return new AtomicNumRange(NumCut.min(getMax(), o.getMin()),
                        NumCut.max(getMax(), o.getMax()));
            } else {
                NumRange sn = new NumRange(this);
                sn.add(o);
                LOGGER.debug("sn {}", sn);
                return sn;
            }
        } else {
            return new AtomicNumRange(NumCut.min(getMax(), o.getMin()), NumCut.max
                    (getMax(), o.getMax()));
        }

    }

    public NumRange minus(Range dother) {
        assert dother instanceof AtomicNumRange;

        AtomicNumRange o = (AtomicNumRange)dother;

        NumRange nr = new NumRange();
        //Set<AtomicNumRange> ret = new HashSet<AtomicNumRange>();
        // [3,5] - [1,3] = [4,5]
        // [1,3] - [3,5] = [1,2]
        // overlap = [3,3]

        // [1,5] - [2,2] = [1,1], [3,5]
        // [2,2] - [1,5] = empty
        // overlap = [2,2]

        // [1,10] - [5,15] = [1,4]
        // [5,15] - [1,10] = [11,15]
        // overlap = [5,10]

        // [1,3] - [7,10] = emtpy

        AtomicNumRange overlap = intersect(o);

        if(this.equals(overlap) || overlap == null)
            return null;


        if(overlap.getMin().isGreaterThan(getMin()) &&
                overlap.getMax().isSmallerThan(getMax())) {

            nr.add(new AtomicNumRange(this.getMin(), overlap.getMin().sub
                    (1L)));
            nr.add(new AtomicNumRange(overlap.getMax().add(1L), this.getMax()));
        } else if (this.getMin().isSmallerThan(overlap.getMin())) {
            // right
            nr.add(new AtomicNumRange(getMin(), overlap.getMin().sub(1L)));
        } else if (this.getMax().isGreaterThan(overlap.getMax())) {
            // left
            nr.add(new AtomicNumRange(overlap.getMax().add(1L), getMax()));
        }
        return nr;
    }

    public NumCut getDiff() {
        LOGGER.debug("DIFF {} - {}: {}", getMax(), getMin(), getMax().diff
                (getMin()));
        return getMax().diff(getMin());
    }

    public AtomicNumRange intersect(Range dother) {

        assert dother instanceof AtomicNumRange;

        AtomicNumRange other = (AtomicNumRange)dother;

        LOGGER.debug("get overlap " + this.toString() + " " + other.toString
                 ());

        if(other.getMin().isGreaterThan(getMax()) || other.getMax()
                .isSmallerThan(getMin()))
            return null;

        AtomicNumRange overlap = new AtomicNumRange(NumCut.max(getMin(), other
                .getMin()),
                NumCut.min(getMax(), other.getMax()));
        LOGGER.debug("Overlap is "+ overlap);

        assert overlap.getMax().isGreaterEqualsThan(overlap.getMin());

        return overlap;
    }


    public boolean equals(long min, long max) {
        return this.equals(new AtomicNumRange(min,max));
    }


    @Override
    public boolean equals(Object o) {
        if(!(o instanceof AtomicNumRange))
            return false;

        AtomicNumRange r = (AtomicNumRange)o;
        return r.getMin().equals(this.getMin()) && r.getMax().equals(this
                .getMax());
    }

    private AtomicNumRange createRange(NumCut [] el) {
        NumCut min = NumCut.min(el);
        NumCut max = NumCut.max(el);
        return new AtomicNumRange(min, max);
    }

    public AtomicNumRange numadd(AtomicNumRange other) {
        NumCut [] el = new NumCut[4];
        el[0] = lb.add(other.lb);
        el[1] = lb.add(other.ub);
        el[2] = ub.add(other.lb);
        el[3] = ub.add(other.ub);
        return createRange(el);
    }

    public AtomicNumRange numsub(AtomicNumRange other) {
        NumCut [] el = new NumCut[4];
        el[0] = lb.sub(other.lb);
        el[1] = lb.sub(other.ub);
        el[2] = ub.sub(other.lb);
        el[3] = ub.sub(other.ub);
        return createRange(el);
    }

    public AtomicNumRange getPosSubRange(){
        return intersect(N);
    }

    public AtomicNumRange getNegSubRange() {
        AtomicNumRange zran = Z.clone();
        zran.setMax(-1);
        return intersect(zran);
    }

    public AtomicNumRange addToLowerBound(long val) {
        return addToLowerBound(new NumCut(val));
    }

    public AtomicNumRange addToLowerBound(NumCut val) {
        NumCut ret = lb.add(val);

        LOGGER.debug("-----------");
        LOGGER.debug("ret {}", ret);
        NumCut min = NumCut.min(ret, getMax());
        NumCut max = NumCut.max(ret, getMax());

        LOGGER.debug("min {}", min);
        LOGGER.debug("max {}", max);
        return new AtomicNumRange(min, max);
    }

    public AtomicNumRange addToUpperBound(long val) {
        return addToUpperBound(new NumCut(val));
    }

    public AtomicNumRange addToUpperBound(NumCut val) {
        NumCut ret = ub.add(val);
        NumCut min = NumCut.min(ret, getMin());
        NumCut max = NumCut.max(ret, getMin());
        return new AtomicNumRange(min, max);
    }

    @Override
    public AtomicNumRange clone() {
        return new AtomicNumRange(this);
    }


}
