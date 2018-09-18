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

import com.github.julianthome.ranger.AtomicNumRange;
import com.github.julianthome.ranger.NumRange;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Random;

public class TestRexpGenFromRange {

    final static Logger LOGGER = LoggerFactory.getLogger(TestRexpGenFromRange.class);

    private AtomicNumRange getRandAtomicRange() {
        long min = new Random().nextInt();
        long max = min + Math.abs(new Random().nextInt(100));
        return new AtomicNumRange(min,max);
    }



    private NumRange getRandNumRange() {
        NumRange r = new NumRange();
        for(int i = 0; i < 5; i++) {
            AtomicNumRange ar = getRandAtomicRange();
            LOGGER.debug("add atomic {}", ar.toString());
            r.add(getRandAtomicRange());
        }
        return r;
    }


    private void testNumRange(NumRange nr) {
        for(AtomicNumRange ar : nr.getRangeMap().values()) {
            testAtomicRange(ar, nr.toRegex());
        }
    }

    private void testAtomicRange(AtomicNumRange ar, String grexp) {

        for(long i = ar.getMin().getEndpoint(); i < ar.getMax().getEndpoint();
            i++ ) {
            String rexp = ar.toRegex();
            //LOGGER.debug(rexp);
            Assertions.assertTrue(String.valueOf(i).matches(rexp));
            Assertions.assertTrue(String.valueOf(i).matches(grexp));
        }

    }


    @Test
    public void testRexpGenForNumRange() {
        for(int i = 0; i < 20; i ++) {
            NumRange nr = getRandNumRange();
            testNumRange(nr);
        }
    }



}
