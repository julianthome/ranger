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

public class BooleanCut extends NumCut {

    private final String sval;

    public static BooleanCut TRUE = new BooleanCut(0,"true");
    public static BooleanCut FALSE = new BooleanCut(1,"false");


    public BooleanCut(long ival, String sval) {
        super(ival);
        this.sval = sval;
    }

    public long getId() {
        return this.endpoint;
    }

    public String toString() {
        return this.sval;
    }

    public static BooleanCut KindFromString(String kind) {
        switch(kind) {
            case "true" : return TRUE.clone();
            case "false" : return FALSE.clone();
            case "0": return TRUE.clone();
            case "1": return FALSE.clone();
        }
        assert(false);
        return null;

    }

    @Override
    public BooleanCut negate() {
        if(isTrue()){
            return FALSE.clone();
        }
        return TRUE.clone();
    }


    @Override
    public BooleanCut clone() {
        return new BooleanCut(this.endpoint, this.sval);
    }

    public String getValue() {
        return this.sval;
    }

    public boolean isTrue(){
        return this.endpoint.equals(0L);
    }

    public boolean isFalse(){
        return !isTrue();
    }


}
