package org.getshaka.shaka
package router

import org.getshaka.shaka.{Binding, Component, ComponentBuilder, OpenState, State, useState}
import scala.collection.mutable.{Buffer, HashMap}
import scala.collection.Seq
import scala.scalajs.js
import scala.scalajs.js.URIUtils
import scala.scalajs.js.annotation.JSGlobal
import scala.util.matching.Regex
import org.scalajs.dom.{Element => _, MouseEvent => _MouseEvent, *}
import org.scalajs.dom.raw.HTMLAnchorElement

private case class Route(regex: Regex, component: Component, paramStates: Seq[OpenState[String]])

class Router(root: String = "/") extends Component:
  private val routes = Buffer.empty[Route]
  private var catchAll: Component = null

  def route(path: Regex, component: Component): Router =
    routes += Router.registerRoute(root, path, component)
    this

  def route(routableComponent: Routable & Component): Router =
    routes += Router.registerRoute(root, routableComponent.path, routableComponent)
    this

  def catchAll(component: Component): Router =
    catchAll = component
    this

  override val template: ComponentBuilder =
    Router.PathState.bind(newPath =>
      routes.find(_.regex.matches(newPath)) match
        case Some(Route(regex, component, paramStates)) =>
          component.render
          for (param, paramState) <- regex.findAllIn(newPath).zip(paramStates) do
              paramState.setValue(param)
          newPath match
            case Router.HashRegex(hashId) =>
              val elementToScroll = document.getElementById(hashId).as[Element | Null]
              if elementToScroll != null then elementToScroll.scrollIntoView()
              else window.scrollTo(0, 0)
            case _ =>
              window.scrollTo(0, 0)
        case None =>
          if catchAll != null then catchAll.render
    )

object Router:
  private[router] val RouteStates = HashMap.empty[String, Seq[OpenState[String]]]
  private val PathState = useState(currentPath)
  private val HashRegex = raw".*#(\S+)".r.anchored
  private val Origin =
    window.location.origin.getOrElse(
      window.location.protocol + "//" + window.location.host
    )

  document.addEventListener("DOMContentLoaded", x => updatePathState(x))
  document.body.addEventListener("click", x => handleClick(x))
  window.addEventListener("popstate", x => updatePathState(x))

  private def currentPath: String =
    var uri = window.location.pathname.stripSuffix("/")
    val hash = window.location.hash
    if (!hash.isEmpty) uri += "/" + hash
    uri = URIUtils.decodeURI(uri)
    if uri.isEmpty then "/" else uri
      
  private def updatePathState[T <: Event](e: T): Unit =
    PathState.setValue(currentPath)
      
  private def handleClick(e: MouseEvent): Unit =
    if e.defaultPrevented
      || e.button != 0
      || e.metaKey
      || e.ctrlKey
      || e.shiftKey
    then return
  
    val anchorOpt =
      e.composedPath()
        .find{
          case x: Element => x.tagName == "A"
          case _ => false
        }
        .map(_.asInstanceOf[HTMLAnchorElement])
    if anchorOpt.isEmpty then return
    val anchor = anchorOpt.get
  
    // https://github.com/lampepfl/dotty/issues/11632
    if (anchor.target != null && !anchor.target.isEmpty)
      || anchor.hasAttribute("download")
      || {
        val rel = anchor.getAttribute("rel")
        rel != null && rel == "external"
      }
    then return
  
    val href = anchor.href
    if href == null
      || href.isEmpty
      || href.contains("mailto:")
      || !href.startsWith(Origin)
    then return
  
    e.preventDefault()
    if href != window.location.href then
      window.history.pushState(js.Object(), "", href)
      updatePathState(e)
  end handleClick

  private[router] def fullRegexString(root: String, path: Regex): String =
    var fixedRoot = root
    if !root.startsWith("/") then fixedRoot = "/" + fixedRoot
    if !fixedRoot.endsWith("/") then fixedRoot += "/"
    // ends with a non-capturing group to allow hashes
    "^" + fixedRoot + path.unanchored.regex.stripPrefix("/").stripSuffix("/") + "(?:/#.*)?"
  
  private[router] def buildParamStates(fullRegexString: String): Seq[OpenState[String]] =
    // count the # groups, attempting to ignore match groups.
    val numGroups = fullRegexString.sliding(2).count(w => w != raw"\(" && w.endsWith("("))
    val paramStates = Buffer.fill(numGroups)(useState(""))
    RouteStates.put(fullRegexString, paramStates)
    paramStates

  private def registerRoute(root: String, path: Regex, component: Component): Route =
    val frs = fullRegexString(root, path)
    RouteStates.get(frs)
      .map(paramStates => Route(frs.r, component, paramStates))
      .getOrElse(Route(frs.r, component, buildParamStates(frs)))

end Router

def useParams(root: String = "/", routable: Routable): Seq[State[String]] =
  val frs = Router.fullRegexString(root, routable.path)
  Router.RouteStates.getOrElse(frs, Router.buildParamStates(frs))

@js.native
private trait MouseEvent extends _MouseEvent:
  def composedPath(): js.Array[EventTarget] = js.native