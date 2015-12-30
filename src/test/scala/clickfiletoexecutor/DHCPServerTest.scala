package clickfiletoexecutor

import org.change.parser.clickfile.ClickToAbstractNetwork
import org.change.v2.executor.clickabstractnetwork.ClickExecutionContext
import org.scalatest.{Matchers, FlatSpec}

/**
 * Author: Radu Stoenescu
 * Don't be a stranger,  symnetic.7.radustoe@spamgourmet.com
 */
class DHCPServerTests  extends FlatSpec with Matchers {

  "A dhcp server click" should "emit a dhcp server offer or release an ip correctly" in {
    val absNet =
ClickToAbstractNetwork.buildConfig("src/main/resources/click_test_files/DHCPServer.click")
    val executor = ClickExecutionContext.fromSingle(absNet)

    var crtExecutor = executor
    while(! crtExecutor.isDone) {
      crtExecutor = crtExecutor.execute()
    }

    crtExecutor.stuckStates should have length (1)

    crtExecutor.stuckStates(0).history.head should be ("toDevice-0-out")

    crtExecutor.failedStates should have length (1)
  }
}
