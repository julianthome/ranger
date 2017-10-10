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

public class AboveAll extends NumCut {

    public AboveAll(Long c) {
        super(c);
    }

    public AboveAll() {
        super(0L);
    }

    @Override
    public boolean isSmallerThan(Cut<Long> val) {
        if (val instanceof AboveAll) {
            AboveAll l = (AboveAll)val;
            return this.endpoint < l.endpoint;
        }
        return isSmallerThan(val.endpoint);
    }

    @Override
    public boolean isSmallerEqualsThan(Cut<Long> val) {
        if (val instanceof AboveAll) {
            AboveAll l = (AboveAll)val;
            return this.endpoint <= l.endpoint;
        }
        return isSmallerEqualsThan(val.endpoint);
    }

    @Override
    public boolean isGreaterThan(Cut<Long> val) {
        if (val instanceof AboveAll) {
            AboveAll l = (AboveAll)val;
            return this.endpoint > l.endpoint;
        }
        return isGreaterThan(val.endpoint);
    }

    @Override
    public boolean isGreaterEqualsThan(Cut<Long> val) {
        if (val instanceof AboveAll) {
            AboveAll l = (AboveAll)val;
            return this.endpoint >= l.endpoint;
        }
        return isGreaterEqualsThan(val.endpoint);
    }

    @Override
    public boolean isSmallerThan(Long value) {
        return false;
    }

    @Override
    public boolean isSmallerEqualsThan(Long value) {
        return false;
    }

    @Override
    public boolean isGreaterThan(Long value) {
        return true;
    }

    @Override
    public boolean isGreaterEqualsThan(Long value) {
        return true;
    }

    @Override
    public NumCut sub(Long val) {
        if(endpoint != null)
            return new AboveAll(endpoint - val);

        return new AboveAll(- val);
    }

    @Override
    public NumCut sub(Cut<Long> val) {
        if(val instanceof AboveAll) {
            return new NumCut(endpoint - val.endpoint);
        } else {
            return sub(val.endpoint);
        }
    }

    @Override
    public NumCut add(Long val) {
        return new AboveAll(endpoint + val);
    }

    @Override
    public NumCut add(Cut<Long> val) {
        if(val instanceof BelowAll) {
            return new NumCut(endpoint + val.endpoint);
        } else {
            return add(val.endpoint);
        }
    }

    @Override
    public BelowAll negate() {
        return new BelowAll(-endpoint);
    }

    @Override
    public String toString() {
        return "+\u221e" + super.toString();
    }

    @Override
    public boolean isFixed() {
        return false;
    }

    @Override
    public NumCut clone() {
        return new AboveAll(this.endpoint);
    }

    @Override
    public boolean isAboveAll() {
        return true;
    }

    @Override
    public boolean equals(Object o) {
        if((o instanceof AboveAll)) {
            AboveAll a = (AboveAll)o;
            return this.endpoint.equals(a.endpoint);
        }
        return false;
    }


}