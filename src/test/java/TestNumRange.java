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


public class TestNumRange {

    final static Logger LOGGER = LoggerFactory.getLogger(TestNumRange.class);

    private void assertion(NumRange rs, int size, long floorEntry, long floorEntryMax, long ceilEntry, long ceilEntryMax) {
        LOGGER.debug(rs.toString());
        assert (rs.size() == size);
        assert (rs.getRangeMap().floorEntry(new NumCut(floorEntry)).getValue()
                .getMax().equals(new NumCut(floorEntryMax)));
        assert (rs.getRangeMap().ceilingEntry(new NumCut(ceilEntry)).getValue()
                .getMax().equals(new NumCut(ceilEntryMax)));
    }

    @Test
    public void simpleRange() {
        AtomicNumRange ar = new AtomicNumRange();

        LOGGER.debug(ar.toString());

        Assert.assertTrue(!ar.isEmpty());
        Assert.assertFalse(ar.isSingleton());
        Assert.assertTrue(!ar.getMin().isFixed());
        Assert.assertTrue(!ar.getMax().isFixed());

        AtomicNumRange nar = ar.addToLowerBound(7L);
        nar = nar.addToLowerBound(10L);
        nar = nar.addToUpperBound(-5L);

        //LOGGER.debug("====");
        nar = nar.addToLowerBound(new AboveAll());
        nar = nar.addToUpperBound(new BelowAll());

        Assert.assertTrue(nar.isBetween(-5L,17L));
    }

    @Test
    public void compareRange() {

        AtomicNumRange ar3 = new AtomicNumRange(0, 100);
        AtomicNumRange ar4 = new AtomicNumRange(-100, 50);

        AtomicNumRange isect2 = ar3.intersect(ar4);
        AtomicNumRange isect3 = ar4.intersect(ar3);


        Assert.assertEquals(isect2.getMin(),new NumCut(0L));
        Assert.assertEquals(isect2.getMax(),new NumCut(50L));
        Assert.assertEquals(isect3, isect2);


        AtomicNumRange ar1 = new AtomicNumRange(1, 200);
        AtomicNumRange ar2 = new AtomicNumRange(-101, 0);

        Assert.assertTrue(ar1.isAlwaysGreaterThan(ar2));
        Assert.assertTrue(ar2.isAlwaysSmallerThan(ar1));

        AtomicNumRange isect1 = ar1.intersect(ar2);
        Assert.assertNull(isect1);

        Range union = ar1.union(ar2);

        Assert.assertTrue(union.getMin().equals(new NumCut(-101L)));
        Assert.assertTrue(union.getMax().equals(new NumCut(200L)));

        LOGGER.debug("+++++++++++++++++++++++++++++++++++++++++++++");
        AtomicNumRange ar5 = new AtomicNumRange(0, 5);
        AtomicNumRange ar6 = new AtomicNumRange(10, 15);

        Range union1 = ar5.union(ar6);

        LOGGER.debug("union {}", union1);

    }

    @Test
    public void testUnion0() {
        NumRange rs = new NumRange(new AtomicNumRange(170,200));
        rs.add(new AtomicNumRange(210,220));
        rs.add(new AtomicNumRange(180,210));
        LOGGER.debug("rs: {}", rs);
        assertion(rs, 1, 170L, 220L, 170L, 220L);
    }

    @Test
    public void testUnion1() {

        NumRange rs = new NumRange(new AtomicNumRange(5, 100));
        rs.add(new AtomicNumRange(160, 180));
        assertion(rs, 2, 5L, 100L, 160L, 180L);
        rs.add(new AtomicNumRange(200, 225));
        assertion(rs, 3, 5L, 100L, 200L, 225L);
        rs.add(new AtomicNumRange(300, 335));
        assertion(rs, 4, 5L, 100L, 300L, 335L);
        rs.add(new AtomicNumRange(370, 400));
        assertion(rs, 5, 5L, 100L, 370L, 400L);
        // [[+5,+100],[+160,+180],[+200,+225],[+300,+335],[+370,+400]]{+5,+400}
        rs.add(new AtomicNumRange(155, 340));
        assertion(rs, 3, 5L, 100L, 370L, 400L);
        // [[+5,+100],[+155,+340],[+370,+400]]{+5,+400}
        LOGGER.debug
                ("======================================================================");
        rs.add(new AtomicNumRange(101, 369));
        LOGGER.debug("{}", rs);
        assertion(rs, 1, 5L, 400L, 0L, 400L);
    }

    @Test
    public void testUnion2() {
        NumRange rs = new NumRange(new AtomicNumRange(-100, 100));
        rs.add(new AtomicNumRange(-101, 102));
        rs.add(new AtomicNumRange(-103, 102));
        assertion(rs, 1, -103L, 102L, -103L, 102L);
        rs.add(new AtomicNumRange(-200, -157));
        assertion(rs, 2, -200L, -157L, -103L, 102L);
        rs.add(new AtomicNumRange(-300, -201));
        assertion(rs, 2, -300, -157L, -103L, 102L);
        rs.add(new AtomicNumRange(-500, 500));
        assertion(rs, 1, -500L, 500L, -500L, 500L);
        rs.add(new AtomicNumRange(-501, 500));
        assertion(rs, 1, -501L, 500L, -501L, 500L);
        rs.add(new AtomicNumRange(-501, 501));
        assertion(rs, 1, -501L, 501L, -501L, 501L);
    }

    @Test
    public void testUnion3() {
        NumRange rs = new NumRange(new AtomicNumRange(1, 1));
        rs.add(new AtomicNumRange(2, 2));
        rs.add(new AtomicNumRange(3, 3));
        rs.add(new AtomicNumRange(4, 4));
        rs.add(new AtomicNumRange(5, 5));
        rs.add(new AtomicNumRange(5, 5));
        assertion(rs, 1, 1L, 5L, 1L, 5L);
    }

    @Test
    public void testUnion4() {
        NumRange rs = new NumRange(new AtomicNumRange(1, 1));
        rs.add(new AtomicNumRange(3, 3));
        rs.add(new AtomicNumRange(5, 5));
        rs.add(new AtomicNumRange(7, 7));
        rs.add(new AtomicNumRange(9, 9));
        rs.add(new AtomicNumRange(11, 11));
        rs.add(new AtomicNumRange(13, 13));
        rs.add(new AtomicNumRange(15, 15));
        rs.add(new AtomicNumRange(17, 17));
        rs.add(new AtomicNumRange(19, 19));
        rs.add(new AtomicNumRange(21, 21));

        assertion(rs, 11, 1L, 1L, 21L, 21L);
        rs.add(new AtomicNumRange(4, 4));
        assertion(rs, 10, 1L, 1L, 21L, 21L);
        rs.add(new AtomicNumRange(0, 1));
        assertion(rs, 10, 0L, 1L, 21L, 21L);
        rs.add(new AtomicNumRange(0, 3));
        assertion(rs, 9, 0L, 5L, 21L, 21L);
        rs.add(new AtomicNumRange(21, 150));
        assertion(rs, 9, 0L, 5L, 21L, 150L);
        rs.add(new AtomicNumRange(11, 17));
        assertion(rs, 6, 0L, 5L, 21L, 150L);
        rs.add(new AtomicNumRange(9, 10));
        assertion(rs, 5, 0L, 5L, 21L, 150L);
        rs.add(new AtomicNumRange(19, 20));
        assertion(rs, 4, 0L, 5L, 19L, 150L);
        rs.add(new AtomicNumRange(100, 121));
        assertion(rs, 4, 0L, 5L, 19L, 150L);
        rs.add(new AtomicNumRange(-300, 300));
        assertion(rs, 1, -300L, 300L, -300L, 300L);
        LOGGER.debug(rs.toString());
    }

    @Test
    public void testUnion5() {
        NumRange rs = new NumRange(new AtomicNumRange(10, 20));
        rs.add(new AtomicNumRange(40, 50));
        rs.add(new AtomicNumRange(70, 80));
        rs.add(new AtomicNumRange(100, 110));

        assertion(rs, 4, 10L, 20L, 100L, 110L);
        rs.add(new AtomicNumRange(51, 69));
        assertion(rs, 3, 10L, 20L, 100L, 110L);
        rs.add(new AtomicNumRange(10, 20));
        assertion(rs, 3, 10L, 20L, 100L, 110L);
        rs.add(new AtomicNumRange(100, 110));
        assertion(rs, 3, 10L, 20L, 100L, 110L);

        LOGGER.debug(rs.toString());
    }

    @Test
    public void testUnion6() {
        NumRange rs = new NumRange(new AtomicNumRange(1, 1));
        rs.add(new AtomicNumRange(3, 3));
        rs.add(new AtomicNumRange(5, 5));
        rs.add(new AtomicNumRange(7, 7));
        rs.add(new AtomicNumRange(9, 9));
        rs.add(new AtomicNumRange(11, 11));
        rs.add(new AtomicNumRange(13, 13));
        rs.add(new AtomicNumRange(15, 15));
        rs.add(new AtomicNumRange(17, 17));
        rs.add(new AtomicNumRange(19, 19));
        rs.add(new AtomicNumRange(21, 21));

        rs.add(new AtomicNumRange(4, 4));

        LOGGER.debug(rs.toString());
    }



    @Test
    public void testUnion8() {
        NumRange n0 = new NumRange(new AtomicNumRange(1, 1));
        n0.add(new AtomicNumRange(3, 3));
        n0.add(new AtomicNumRange(5, 5));
        n0.add(new AtomicNumRange(7, 7));
        n0.add(new AtomicNumRange(9, 9));

        NumRange n1 = new NumRange(new AtomicNumRange(-1, -1));
        n1.add(new AtomicNumRange(3, 5));


        LOGGER.debug(n0.toString());
        LOGGER.debug(n1.toString());

        NumRange add = n0.numadd(n1);
        LOGGER.debug(add.toString());

    }


    @Test
    public void testIntersection0() {
        NumRange rs0 = new NumRange(new AtomicNumRange(1, 1));

        rs0.add(new AtomicNumRange(3, 3));
        rs0.add(new AtomicNumRange(5, 5));
        rs0.add(new AtomicNumRange(7, 7));
        rs0.add(new AtomicNumRange(9, 9));

        NumRange rs1 = new NumRange(new AtomicNumRange(1, 3));

        NumRange isect = rs0.intersect(rs1);

        assertion(isect, 2, 1L, 1L, 2L, 3L);
    }

    @Test
    public void testIntersection1() {
        NumRange rs0 = new NumRange(new AtomicNumRange(1, 1));
        rs0.add(new AtomicNumRange(3, 3));
        rs0.add(new AtomicNumRange(5, 5));
        rs0.add(new AtomicNumRange(7, 7));
        rs0.add(new AtomicNumRange(9, 9));
        rs0.add(new AtomicNumRange(11, 11));
        rs0.add(new AtomicNumRange(13, 13));
        rs0.add(new AtomicNumRange(15, 15));
        rs0.add(new AtomicNumRange(17, 17));
        rs0.add(new AtomicNumRange(19, 19));
        rs0.add(new AtomicNumRange(21, 21));

        NumRange rs1 = new NumRange(new AtomicNumRange(1, 1));
        rs1.add(new AtomicNumRange(3, 3));
        rs1.add(new AtomicNumRange(5, 5));
        rs1.add(new AtomicNumRange(7, 7));
        rs1.add(new AtomicNumRange(9, 9));
        rs1.add(new AtomicNumRange(11, 11));
        rs1.add(new AtomicNumRange(13, 13));
        rs1.add(new AtomicNumRange(15, 15));
        rs1.add(new AtomicNumRange(17, 17));
        rs1.add(new AtomicNumRange(19, 19));
        rs1.add(new AtomicNumRange(21, 21));

        NumRange isect = rs0.intersect(rs1);

        Assert.assertEquals(rs1,isect);

        NumRange rs2 = new NumRange(new AtomicNumRange(1, 1));
        rs2.add(new AtomicNumRange(3, 3));
        rs2.add(new AtomicNumRange(5, 5));
        rs2.add(new AtomicNumRange(7, 7));
        rs2.add(new AtomicNumRange(9, 9));

        rs2.add(new AtomicNumRange(17, 17));
        rs2.add(new AtomicNumRange(19, 19));
        rs2.add(new AtomicNumRange(21, 21));

        Assert.assertEquals(rs2.intersect(rs1),rs2);

    }

    @Test
    public void testIntersection2() {
        NumRange rs0 = new NumRange(new AtomicNumRange(1, 100));
        rs0.add(new AtomicNumRange(300, 350));
        rs0.add(new AtomicNumRange(500, 5000));


        NumRange rs1 = new NumRange(new AtomicNumRange(-199, 10));
        rs1.add(new AtomicNumRange(100, 176));
        rs1.add(new AtomicNumRange(460, 10000));

        NumRange isect = rs1.intersect(rs0);

        LOGGER.debug(isect.toString());

    }

    @Test
    public void testSetMin() {
        NumRange rs0 = new NumRange(new AtomicNumRange(1, 100));
        rs0.add(new AtomicNumRange(300, 350));
        rs0.add(new AtomicNumRange(500, 5000));


        NumRange rs1 = new NumRange(new AtomicNumRange(-199, 10));
        rs1.add(new AtomicNumRange(100, 176));
        rs1.add(new AtomicNumRange(460, 10000));

        NumRange isect = rs1.intersect(rs0);
        //(1)[1,10],[100,176],[460,10000](10000)

        isect.setMin(11);

        LOGGER.debug(isect.toString());

    }

    @Test
    public void testIsect() {
        NumRange rs0 = new NumRange(new AtomicNumRange(0, 100));


        LOGGER.debug("RS 0" + rs0.toString());
        NumRange rs1 = new NumRange(new AtomicNumRange(0, 50));
        rs1.add(new AtomicNumRange(52, 100));
        LOGGER.debug("RS 1 " + rs1.toString());

        NumRange out = rs0.intersect(rs1);

        LOGGER.debug(">> " + out.toString());

    }

    @Test
    public void testEqualIsect() {
        NumRange rs0 = new NumRange(new AtomicNumRange(36, 36));

        NumRange rs1 = new NumRange(new AtomicNumRange(36, 36));

        LOGGER.debug("RS 1 " + rs1.toString());

        NumRange out = rs0.intersect(rs1);
        NumRange out2 = out.intersect(rs0);

        LOGGER.debug(">> " + out2.toString());

    }

}
