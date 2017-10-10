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

import com.github.julianthome.ranger.AboveAll;
import com.github.julianthome.ranger.BelowAll;
import com.github.julianthome.ranger.NumCut;
import org.junit.Assert;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


public class TestCut {

    final static Logger LOGGER = LoggerFactory.getLogger(TestCut.class);


    @Test
    public void testNumCut() {

        NumCut nc1 = new NumCut(100L);
        NumCut nc2 = new NumCut(101L);
        NumCut nc3 = new NumCut(101L);
        NumCut above1 = new AboveAll();
        NumCut above2 = new AboveAll(1L);
        NumCut above3 = new AboveAll(-1L);

        NumCut below1 = new BelowAll();
        NumCut below2 = new BelowAll(1L);
        NumCut below3 = new BelowAll(-1L);


        Assert.assertTrue(nc1.isSmallerThan(nc2));
        Assert.assertTrue(nc1.isSmallerEqualsThan(nc2));
        Assert.assertTrue(nc2.isGreaterThan(nc1));
        Assert.assertTrue(nc2.isGreaterEqualsThan(nc2));
        Assert.assertFalse(nc1.equals(nc2));
        Assert.assertTrue(nc3.equals(nc2));
        Assert.assertTrue(nc3.equals(nc3));

        Assert.assertTrue(above1.isGreaterEqualsThan(nc1));
        Assert.assertTrue(above1.isGreaterEqualsThan(nc2));
        Assert.assertTrue(above1.isGreaterEqualsThan(nc3));
        Assert.assertTrue(above2.isGreaterEqualsThan(above1));
        Assert.assertTrue(above3.isSmallerEqualsThan(above1));
        Assert.assertTrue(above1.isAboveAll());
        Assert.assertFalse(above1.isBelowAll());
        Assert.assertFalse(above1.isFixed());

        Assert.assertTrue(nc1.isSmallerThan(above1));
        Assert.assertTrue(nc2.isSmallerThan(above1));
        Assert.assertTrue(nc3.isSmallerThan(above1));


        Assert.assertNotEquals(above1, above2);
        Assert.assertTrue(above2.isGreaterThan(above1));
        Assert.assertTrue(below1.isSmallerThan(below2));
        Assert.assertTrue(below3.isSmallerThan(below2));
        Assert.assertTrue(below1.isBelowAll());
        Assert.assertFalse(below1.isAboveAll());
        Assert.assertFalse(below1.isFixed());


        LOGGER.debug("min {}", nc1.compareTo(nc2));

    }
}
