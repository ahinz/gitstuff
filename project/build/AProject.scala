package hobby.distr.project.build
import sbt._ 
 
class AProject(info: ProjectInfo) extends DefaultProject(info) { 
      val scalatest = "org.scalatest" % "scalatest" % "1.3"
    }
// vim: set ts=4 sw=4 et:
