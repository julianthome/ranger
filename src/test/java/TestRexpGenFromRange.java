import com.github.julianthome.ranger.AtomicNumRange;
import com.github.julianthome.ranger.NumRange;
import org.junit.Assert;
import org.junit.Test;
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
            Assert.assertTrue(String.valueOf(i).matches(rexp));
            Assert.assertTrue(String.valueOf(i).matches(grexp));
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
