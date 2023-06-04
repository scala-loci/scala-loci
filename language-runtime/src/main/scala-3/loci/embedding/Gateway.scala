package loci
package embedding

import scala.language.dynamics

trait Gateway[+R]
//  extends Dynamic
//  def selectDynamic(key: String): Unit = macro GatewayResolutionFailure.selectDynamic
//  def updateDynamic(key: String)(value: Any): Unit = macro GatewayResolutionFailure.updateDynamic
//  def applyDynamic(key: String)(args: Any*): Unit = macro GatewayResolutionFailure.applyDynamic
//  def applyDynamicNamed(key: String)(args: Any*): Unit = macro GatewayResolutionFailure.applyDynamic

object Gateway:
//  extends language.transmitter.RemoteGateway.Default
  abstract class Implementation[+R] private[loci] extends Gateway[R]
