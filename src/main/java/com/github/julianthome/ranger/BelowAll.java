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

public class BelowAll extends NumCut {

    public BelowAll(Long l) {
        super(l);
    }

    public BelowAll() {
        this(0L);
    }

    @Override
    public boolean isSmallerThan(Cut<Long> val) {
        if (val instanceof BelowAll) {
            BelowAll l = (BelowAll)val;
            return this.endpoint < l.endpoint;
        }
        return isSmallerThan(val.endpoint);
    }

    @Override
    public boolean isSmallerEqualsThan(Cut<Long> val) {
        if (val instanceof BelowAll) {
            BelowAll l = (BelowAll)val;
            return this.endpoint <= l.endpoint;
        }
        return isSmallerEqualsThan(val.endpoint);
    }

    @Override
    public boolean isGreaterThan(Cut<Long> val) {
        if (val instanceof BelowAll) {
            BelowAll l = (BelowAll)val;
            return this.endpoint > l.endpoint;
        }
        return isGreaterThan(val.endpoint);
    }

    @Override
    public boolean isGreaterEqualsThan(Cut<Long> val) {
        if (val instanceof BelowAll) {
            BelowAll l = (BelowAll)val;
            return this.endpoint >= l.endpoint;
        }
        return isGreaterEqualsThan(val.endpoint);
    }

    @Override
    public boolean isSmallerThan(Long value) {
        return true;
    }

    @Override
    public boolean isSmallerEqualsThan(Long value) {
        return true;
    }

    @Override
    public boolean isGreaterThan(Long value) {
        return false;
    }

    @Override
    public boolean isGreaterEqualsThan(Long value) {
        return false;
    }

    @Override
    public AboveAll negate() {
        return new AboveAll(-endpoint);
    }

    @Override
    public NumCut sub(Long val) {
        return new BelowAll(endpoint - val);
    }

    @Override
    public NumCut sub(Cut<Long> val) {
        if(val instanceof BelowAll) {
            return new NumCut(endpoint - val.endpoint);
        } else {
            return sub(val.endpoint);
        }
    }

    @Override
    public NumCut add(Long val) {
        return new BelowAll(endpoint + val);
    }

    @Override
    public NumCut add(Cut<Long> val) {
        LOGGER.debug("add {} to {}", val, this);
        if(val instanceof AboveAll) {
            return new NumCut(endpoint + val.endpoint);
        } else {
            return add(val.endpoint);
        }
    }


    @Override
    public String toString() {
        return "-\u221e" + super.toString();
    }

    @Override
    public boolean isFixed() {
        return false;
    }

    @Override
    public int hashCode() {
        return endpoint.hashCode();
    }

    @Override
    public NumCut clone() {
        return new BelowAll(this.endpoint);
    }

    @Override
    public boolean isBelowAll() {
        return true;
    }

    @Override
    public boolean equals(Object o) {
        if((o instanceof BelowAll)) {
            BelowAll a = (BelowAll)o;
            return this.endpoint.equals(a.endpoint);
        }
        return false;
    }

}