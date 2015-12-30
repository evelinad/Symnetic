package clickfiletoexecutor

import org.change.parser.clickfile.ClickToAbstractNetwork
import org.change.v2.executor.clickabstractnetwork.ClickExecutionContext
import org.scalatest.{Matchers, FlatSpec}

/**
 * Author: Radu Stoenescu
 * Don't be a stranger,  symnetic.7.radustoe@spamgourmet.com
 */
class DHCPClientTests  extends FlatSpec with Matchers {

  "A dhcp client click" should "emit a generic dhcp message correctly" in {
    val absNet =
ClickToAbstractNetwork.buildConfig("src/main/resources/click_test_files/DHCPClient.click")
    val executor = ClickExecutionContext.fromSingle(absNet, initialIsClean = true)

    var crtExecutor = executor
    while(! crtExecutor.isDone) {
      crtExecutor = crtExecutor.execute()
    }

    crtExecutor.stuckStates should have length (1)

    crtExecutor.stuckStates(0).history.head should be ("toDevice-37-out")

    crtExecutor.failedStates should have length (1)
  }
}
