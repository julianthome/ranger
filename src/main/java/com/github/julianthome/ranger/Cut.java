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


import java.io.Serializable;

abstract class Cut<C extends Comparable> implements Comparable<Cut<C>>,
        Serializable {

    protected C endpoint;

    protected Cut(Cut<C> c) {
        this.endpoint = c.endpoint;
    }

    protected Cut(C c) {
        this.endpoint = c;
    }

    protected Cut() {
    }


    public abstract boolean isSmallerThan(Cut<C> value);
    public abstract boolean isSmallerEqualsThan(Cut<C> value);
    public abstract boolean isGreaterThan(Cut<C> value);
    public abstract boolean isGreaterEqualsThan(Cut<C> value);

    public abstract boolean isAboveAll();
    public abstract boolean isBelowAll();

    public abstract Cut<C> sub(Cut<C> val);
    public abstract Cut<C> add(Cut<C> val);
    public abstract Cut<C> div(Cut<C> val);
    public abstract Cut<C> mul(Cut<C> val);
    public abstract Cut<C> diff(Cut<C> val);
    public abstract Cut<C> negate();

    @Override
    public int hashCode() {
        return toString().hashCode();
    }
    // note: overriden by {BELOW,ABOVE}_ALL
    @Override
    public int compareTo(Cut<C> o) {
        if (isGreaterThan(o)) {
            return 1;
        }
        if (isSmallerThan(o)) {
            return -1;
        }
        return 0;
    }


    public C getEndpoint() {
        return endpoint;
    }

    @Override
    public abstract boolean equals(Object obj);

    abstract boolean isFixed();

}
