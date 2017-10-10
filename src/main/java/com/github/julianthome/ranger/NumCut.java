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

public class NumCut extends Cut<Long> implements Cloneable {

    final static Logger LOGGER = LoggerFactory.getLogger(NumCut.class);


    public static NumCut max(NumCut ... e) {
        assert e.length > 0;
        NumCut ptr = e[0];
        for(int i = 1; i < e.length; i++){
            ptr = max(ptr, e[i]);
        }
        return ptr;
    }

    public static NumCut min(NumCut ... e) {
        assert e.length > 0;
        NumCut ptr = e[0];
        for(int i = 1; i < e.length; i++){
            ptr = min(ptr, e[i]);
        }
        return ptr;
    }

    private static NumCut max(NumCut a, NumCut b) {
        //LOGGER.debug("NumCut max {} -- {}", a, b);
        if(a.isGreaterThan(b)) {
            //LOGGER.debug("1");
            return a;
        } else {
            //LOGGER.debug("2");
            return b;
        }
    }

    private static NumCut min(NumCut a, NumCut b) {
        //LOGGER.debug("NumCut min {} -- {}", a, b);
        if(a.isSmallerThan(b)) {
            //LOGGER.debug("1");
            return a;
        } else {
            //LOGGER.debug("2");
            return b;
        }
    }

    public static NumCut abs(NumCut a) {
        return a.isSmallerThan(0L) ? new NumCut(a.mul(-1L)) : new NumCut(a);
    }

    public NumCut() {
        endpoint = null;
    }

    public NumCut(Long c) {
        super(c);
    }

    public NumCut(Integer c) {
        super(c.longValue());
    }

    public NumCut(Cut<Long> sub) {
        super(sub);
    }



    public boolean isSmallerThan(Cut<Long> value) {
        if(value instanceof AboveAll)
            return true;
        if(value instanceof BelowAll)
            return false;

        return this.endpoint < value.endpoint;
    }
    public boolean isSmallerEqualsThan(Cut<Long> value) {
        if(value instanceof AboveAll)
            return true;
        if(value instanceof BelowAll)
            return false;

        return this.endpoint <= value.endpoint;
    }

    public boolean isGreaterThan(Cut<Long> value){
        if(value instanceof BelowAll)
            return true;
        if(value instanceof AboveAll)
            return false;
        return this.endpoint > value.endpoint;
    }

    public boolean isGreaterEqualsThan(Cut<Long> value){
        if(value instanceof BelowAll)
            return true;
        if(value instanceof AboveAll)
            return false;
        return this.endpoint >= value.endpoint;
    }

    public boolean isSmallerThan(Long value) {
        return isSmallerThan(new NumCut(value));
    }


    public boolean isSmallerEqualsThan(Long value) {
        return isSmallerEqualsThan(new NumCut(value));
    }


    public boolean isGreaterThan(Long value) {
        return isGreaterThan(new NumCut(value));
    }


    public boolean isGreaterEqualsThan(Long value) {
        return isGreaterThan(new NumCut(value));
    }

    @Override
    public boolean isAboveAll() {
        return false;
    }

    @Override
    public boolean isBelowAll() {
        return false;
    }

    @Override
    public NumCut sub(Cut<Long> val) {
        if(val instanceof AboveAll) {
            return ((AboveAll)val).negate().add(this);
        } else if (val instanceof BelowAll) {
            return ((BelowAll)val).negate().add(this);
        } else
            return sub(val.endpoint);
    }

    @Override
    public NumCut add(Cut<Long> val) {
        if(val instanceof AboveAll) {
            return ((AboveAll) val).add(this);
        } else if (val instanceof BelowAll) {
            return ((BelowAll) val).add(this);
        } else
            return add(val.endpoint);
    }

    @Override
    public NumCut div(Cut<Long> val) {
        return div(val.endpoint);
    }

    @Override
    public NumCut mul(Cut<Long> val) {
        return mul(val.endpoint);
    }

    @Override
    public NumCut diff(Cut<Long> val) {
        return diff(val.endpoint);
    }

    @Override
    public Cut<Long> negate() {
        return new NumCut(-endpoint);
    }


    public NumCut sub(Long val) {
        return new NumCut(endpoint - val);
    }


    public NumCut add(Long val) {
        return new NumCut(endpoint + val);
    }


    public NumCut div(Long val) {
        return new NumCut(endpoint/val);
    }

    public NumCut mul(Long val) {
        return new NumCut(endpoint*val);
    }

    public NumCut diff(Long val) {
        return abs(sub(val));
    }

    @Override
    public boolean isFixed() {
        return true;
    }

    @Override
    public NumCut clone() {
        return new NumCut(this.endpoint);
    }

    @Override
    public String toString() {
       return this.endpoint < 0 ? this.endpoint.toString() : "+" + this
               .endpoint.toString();
    }

    @Override
    public boolean equals(Object o) {
        if(o instanceof AboveAll || o instanceof BelowAll)
            return false;

        if((o instanceof NumCut)) {
            NumCut a = (NumCut)o;
            return endpoint.equals(a.endpoint);
        }
        return false;
    }
}
