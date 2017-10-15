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

import com.github.julianthome.ranger.*;
import org.junit.Assert;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Set;


public class TestBasicRange {

    final static Logger LOGGER = LoggerFactory.getLogger(TestBasicRange.class);

    public static BooleanRange trange = new BooleanRange(BooleanCut.TRUE.clone());
    public static BooleanRange frange = new BooleanRange(BooleanCut.FALSE.clone());
    public static BooleanRange vrange = new BooleanRange();


    private void debug(Set<AtomicNumRange> nr) {
        for(AtomicNumRange n : nr) {
            LOGGER.info(">> " + n.toString());
        }
    }


    @Test
    public void testArithmetic() {

        AtomicNumRange a1 = new AtomicNumRange();
        AtomicNumRange a2 = new AtomicNumRange(4,4);
        AtomicNumRange sum = a1.numadd(a2);

        Assert.assertEquals(sum.getMin(), new BelowAll(4L));
        Assert.assertEquals(sum.getMax(), new AboveAll(4L));

        AtomicNumRange diff = a1.numsub(a2);

        Assert.assertEquals(diff.getMin(), new BelowAll(-4L));
        Assert.assertEquals(diff.getMax(), new AboveAll(-4L));

        AtomicNumRange a3 = new AtomicNumRange(10,150);
        AtomicNumRange a4 = new AtomicNumRange(-100,50);

        sum = a3.numadd(a4);
        diff = a3.numsub(a4);

        Assert.assertTrue(sum.isBetween(-90,200));

        LOGGER.debug("diff {}", diff);
        Assert.assertTrue(diff.isBetween(-40,250));


        AtomicNumRange a5 = new AtomicNumRange(new NumCut(1L), new AboveAll());
        AtomicNumRange a6 = new AtomicNumRange(new NumCut(0L), new AboveAll
                (-1L));

        sum = a5.numsub(a6);

        Assert.assertTrue(sum.isBetween(new BelowAll(-1L), new AboveAll()));

        LOGGER.debug("sum {}", sum);

    }

    @Test
    public void testMinus() {

        AtomicNumRange nr0 = new AtomicNumRange(0,100);
        AtomicNumRange nr1 = new AtomicNumRange(50,99);

        NumRange nset0 = nr0.minus(nr1);

        LOGGER.debug("{}", nset0);

        assert(nset0.size() == 2);

        for(AtomicNumRange n : nset0.getRangeMap().values()) {
            assert(n.equals(0,49) || n.equals(100,100));
        }

        AtomicNumRange nr2 = new AtomicNumRange(150, 190);
        AtomicNumRange nr3 = new AtomicNumRange(190, 200);

        NumRange nset1 = nr2.minus(nr3);
        NumRange nset2 = nr3.minus(nr2);

        //debug(nset1);
        //debug(nset2);

        assert(nset1.size() == 1);
        assert(nset2.size() == 1);


        Assert.assertEquals(nset1.getMin(), new NumCut(150L));
        Assert.assertEquals(nset1.getMax(), new NumCut(189L));

        Assert.assertEquals(nset2.getMin(), new NumCut(191L));
        Assert.assertEquals(nset2.getMax(), new NumCut(200L));



        AtomicNumRange nr4 = new AtomicNumRange(1,1000);
        AtomicNumRange nr5 = new AtomicNumRange(0,2000);

        NumRange nset3 = nr4.minus(nr5);
        NumRange nset4 = nr5.minus(nr4);

        assert(nset3 == null);

        for(AtomicNumRange n : nset4.getRangeMap().values()) {
            assert(n.equals(0,0) || n.equals(1001,2000));
        }

    }

    @Test
    public void testBooleanRange() {

        Assert.assertTrue(trange.isAlwaysTrue());
        Assert.assertTrue(frange.isAlwaysFalse());
        Assert.assertTrue(!trange.isCatState());
        Assert.assertTrue(!frange.isCatState());

        Assert.assertTrue(trange.and(trange).isAlwaysTrue());
        Assert.assertTrue(!trange.and(frange).isAlwaysTrue());
        Assert.assertTrue(trange.and(frange).isAlwaysFalse());
        Assert.assertTrue(frange.and(frange).isAlwaysFalse());

        Assert.assertTrue(trange.or(trange).isAlwaysTrue());
        Assert.assertTrue(trange.or(frange).isAlwaysTrue());
        Assert.assertTrue(trange.or(frange).isAlwaysTrue());
        Assert.assertTrue(frange.or(frange).isAlwaysFalse());

        LOGGER.debug("" + frange.xor(trange));

        Assert.assertTrue(frange.xor(trange).isAlwaysTrue());


        BooleanRange ntrange = trange.negate();

        Assert.assertTrue(ntrange.equals(frange));

        trange = ntrange.negate();

        BooleanRange nfrange = frange.negate();

        Assert.assertTrue(trange.equals(nfrange));

        BooleanRange frange = nfrange.negate();

        Assert.assertTrue(vrange.and(trange).isCatState());
        Assert.assertTrue(vrange.and(frange).isAlwaysFalse());

        Assert.assertTrue(vrange.or(trange).isAlwaysTrue());
        Assert.assertTrue(vrange.or(frange).isCatState());

    }


    @Test
    public void testSimple() {
        AtomicNumRange rinf = new AtomicNumRange();
        System.out.println("1: " + rinf.toString());
        AtomicNumRange r4 = new AtomicNumRange(4);
        System.out.println("2: " + r4.toString());
        AtomicNumRange r101 = new AtomicNumRange(100,101);
        System.out.println("3: " + r101.union(r4).toString());
        System.out.println("4: " + rinf.intersect(r4).toString());
        System.out.println("5: " + new AtomicNumRange(3,10).union(new
                AtomicNumRange(2,13)).toString());
        System.out.println("6: " + r101.union(r4).toRegex());

    }

}

