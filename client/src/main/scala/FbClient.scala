
import akka.actor._
import akka.io.IO
import akka.pattern.ask
import akka.util.Timeout
import scala.concurrent.Await
import scala.concurrent.Future
import spray.can.Http
import spray.util._
import spray.http._
import spray.json._
import HttpMethods._
import scala.io._
import scala.util.Random
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import akka.routing.RoundRobinPool
import ClientDesign._
import java.security.SecureRandom
import org.apache.commons.codec.binary.Base64
import java.security._

import fbcrypto._

case class TerminateSimulation()
case class usersCreatework()
case class pagesCreatework()
case class postsCreatework()
case class pagesCreationdone()
case class usersCreationdone()
case class postsCreationdone()

case class createFriendshipwork()
case class createFriendshipdone()

case class usersDeletework()
case class usersDeletiondone()

case class postUser(a: Int, b: Int, publicKey: PublicKey)
case class CreatePage(a: Int, b: Int, c: Int)
case class createPost(a: Int, b: Int, c: Int)
case class createPostForPage(a: Int, b: Int, c: Int, d: Int)
case class createFriendshipNetwork(a: Int, b: Int, c: Int)

case class deleteUsers(a: Int, b: Int)

class Master(workerCount: Int, countUsers: Int, countPosts: Int, countPages: Int) extends Actor with ActorLogging {
  val ISO_CONST: String = "ISO-8859-1"

  val url = "http://localhost:7005"
  val usr = "shiva"
  val pwd = "shivaok"
  implicit val timeout = Timeout(60 seconds)
  var startTime, endTime: Long = 0
  val workerActor = context.actorOf(Props[Worker].withRouter(RoundRobinPool(workerCount)), name = "Worker")
  var b, d, c: Int = 0
  var f: Int = 0
  /*
       * h() - decrypts encrypted aeskey  using RSA private key
     	 * i() - decrypts encrypted input data using AES
       */
  def h(key: String): String = {

    RSA.decrypt(RSA.getPrivateKey(), key) //rsa decryption of aeskey

  }

  def i(b: String, aesKeyd: String): String = {
    AES.decrypt(b.getBytes(ISO_CONST), aesKeyd.getBytes(ISO_CONST)) //b.getBytes(ISO_CONST), aesKeyd)
  }
  def iso(a: String) = new String((new Base64()).decode(a), ISO_CONST)

  def getUser(id: Int) =
    {
      log.info("getUser method called!")
      val future = IO(Http)(context.system).ask(HttpRequest(GET, Uri(s"$url/users/$id"))).mapTo[HttpResponse]
      //val future = IO(Http)(context.system).ask(HttpRequest(GET, Uri(s"$url/users/$id?usr=$usr&pwd=$pwd"))).mapTo[HttpResponse]

      val usr = Await.result(future, timeout.duration).asInstanceOf[HttpResponse]

      if ((usr.status).toString() == "201 Created" || (usr.status).toString() == "200 OK") {
        log.info("id: " + id.toString + " user status @ get USer: " + usr.status)
      } else {
        log.info("id: " + id.toString + " user status @ get User: " + usr.status)
      }

      var d: Option[Any] = scala.util.parsing.json.JSON.parseFull(usr.entity.data.asString)
      var mp: Map[String, String] = d.get.asInstanceOf[Map[String, String]]

      log.info("user data: " + mp.toString())
      log.info("aeskey just before decrypt: " + iso(mp.get("key").get))

      val aeskey_to_decrypt: String = h(iso(mp.get("key").get))

      log.info("decrypted aeskey: " + aeskey_to_decrypt)

      val decryptval: String = i(iso(mp.get("username").get), aeskey_to_decrypt)
      val decryptval1: String = i(iso(mp.get("name").get), aeskey_to_decrypt)
      log.info("decrypted username: " + decryptval + ", name: " + decryptval1)

    }
  def receive = {
    case usersCreatework() => {
      RSA.genKeys() //called once only

      StartTimer()
      var fair: Int = countUsers / workerCount;
      for (i <- 1 to workerCount) {

        var start: Int = fair * i - (fair - 1)
        var end: Int = fair * i

        workerActor ! postUser(start, end, RSA.getPublicKey())
      }
    }
    case pagesCreatework() => {
      StartTimer()
      val r = new scala.util.Random
      var fair: Int = countPages / workerCount;
      for (i <- 1 to workerCount) {

        var start: Int = fair * i - (fair - 1)
        var end: Int = fair * i

        workerActor ! CreatePage(r.nextInt(countUsers) + 1, start, end)
      }
    }
    case postsCreatework() => {
      StartTimer()
      val r = new scala.util.Random
      var fair: Int = countPosts / workerCount;
      for (i <- 1 to workerCount) {

        var start: Int = fair * i - (fair - 1)
        var end: Int = fair * i

        //send work to createpost which reflects on user feed
        workerActor ! createPost(r.nextInt(countUsers) + 1, start, end)
        //send work to createpostonpage which reflects on page feed
        workerActor ! createPostForPage(r.nextInt(countUsers) + 1, r.nextInt(countPages) + 1, start, end)
      }
    }

    case createFriendshipwork() => {
      StartTimer()
      val r = new scala.util.Random
      var fair: Int = countUsers / workerCount;
      fair = fair / 4;

      for (i <- 1 to workerCount) {

        var start: Int = fair * i - (fair - 1)
        var end: Int = fair * i

        workerActor ! createFriendshipNetwork(start, end, countUsers / 4)
      }
    }
    case usersDeletework() => {
      StartTimer()
      var fair: Int = countUsers / workerCount;
      for (i <- 1 to workerCount) {

        var start: Int = fair * i - (fair - 1)
        var end: Int = fair * i

        workerActor ! deleteUsers(start, end)
      }
    }

    case pagesCreationdone() => {
      d = d + 1
      if (d == workerCount) {
        println("pages Creation done")
        EndTimer()
        // self ! TerminateSimulation()
        self ! postsCreatework()
      }
    }
    case usersCreationdone() => {
      c = c + 1
      if (c == workerCount) {

        println("users Creation done")
        EndTimer()
        for (i <- 1 to workerCount)
          getUser(i)
        self ! TerminateSimulation()
        //self ! pagesCreatework()
        //self ! createFriendshipwork()
      }
    }
    case usersDeletiondone() => {
      c = c + 1
      if (c == workerCount) {
        EndTimer()
        //self ! TerminateSimulation()
        //self ! pagesCreatework()
        //self ! createFriendshipwork()
      }
    }
    case postsCreationdone() => {
      b = b + 1
      if (b == workerCount * 2) {
        println("posts Creation done")
        EndTimer()
        self ! TerminateSimulation()
      }
    }
    case createFriendshipdone() => {
      f = f + 1
      if (f == workerCount) {
        println("create Friendship done")
        EndTimer()
        self ! TerminateSimulation()
      }
    }

    case TerminateSimulation() => {
      println("shutting down the simulator")
      context.system.shutdown()
    }
    case _ => {
      log.info("came to default!")
    }
  }
  private def StartTimer() {
    startTime = System.currentTimeMillis()
  }
  private def EndTimer() {
    endTime = System.currentTimeMillis() - startTime
    println("Total elapsed time:" + endTime)
  }

}

class Worker extends Actor with ActorLogging {

  implicit val timeout = Timeout(60 seconds)
  val url = "http://localhost:7005"
  val usr = "shiva"
  val pwd = "shivaok"

  var publicKey: PublicKey = null
  //  RSA.genKeys() //TODO: need to move it to master!
  val ISO_CONST: String = "ISO-8859-1"
  var aeskey: Array[Byte] = null
  //var aeskey:String="0123456789012345"
  def GenAESKey(): Array[Byte] = {
    var rand = new SecureRandom()
    var aesKeye: Array[Byte] = new Array[Byte](16)
    rand.nextBytes(aesKeye)
    aesKeye
  }
  /* functions 
     * f() - encrypts input data using AES,
     * g() - encrypts aeskey  using RSA public key,
     */

  def f(a: Any): String = {
    //AES.encrypt(a.toString.getBytes(ISO_CONST),aeskey.getBytes(ISO_CONST))
    AES.encrypt(a.toString.getBytes(ISO_CONST), aeskey)
  }

  def g(key: Array[Byte]): String = {
    RSA.encrypt(publicKey, key) //rsa encryption of aeskey
  }

  def b64(a: String) = (new Base64()).encodeAsString(a.getBytes(ISO_CONST))

  def receive = {
    case postUser(start: Int, end: Int, pubKey: PublicKey) => {
      aeskey = GenAESKey()
      publicKey = pubKey;
      // RSA.genKeys()

      for (id <- start to end) {
        var encryptedAes: String = g(aeskey)

        val newUser = new User(id, Some(b64(f("Shivdeep"))), Some(b64(f("Prashanth"))), List(), List(), List(), b64(encryptedAes)).toJson

        log.info("aeskey: " + new String(aeskey, ISO_CONST))
        log.info("encrepted user data: " + newUser.toString())
        log.info("encrypted aeskey: " + encryptedAes)

        val future = IO(Http)(context.system).ask(HttpRequest(PUT, Uri(s"$url/user"), entity = HttpEntity(MediaTypes.`application/json`, newUser.toString))).mapTo[HttpResponse]
        val usr = Await.result(future, timeout.duration).asInstanceOf[HttpResponse]
        if ((usr.status).toString() == "201 Created" || (usr.status).toString() == "200 OK") {
          log.info("id: " + id.toString + " user status @ post user: " + usr.status)
        } else {
          log.info("id: " + id.toString + " user status @ post user: " + usr.status)
        }
      }
      sender ! usersCreationdone()
    }
    case CreatePage(uid: Int, start: Int, end: Int) => {
      for (id <- start to end) {
        val newPage = new Page(id,Some(b64(f("a"))), 0, List(), List()).toJson
        val future = IO(Http)(context.system).ask(HttpRequest(PUT, Uri(s"$url/users/$uid/page"), entity = HttpEntity(MediaTypes.`application/json`, newPage.toString))).mapTo[HttpResponse]
        val page = Await.result(future, timeout.duration).asInstanceOf[HttpResponse]
        //log.info("page-" + (page.status).toString())

        if ((page.status).toString() == "201 Created" || (page.status).toString() == "200 OK") {
          //log.info("id: " + id.toString + " uid -" + uid.toString() + " 201 page")
        } else {
          log.info("id: " + id.toString + " uid -" + uid.toString() + "page status: " + page.status)
        }
      }
      sender ! pagesCreationdone()
    }
    case createPost(uid: Int, start: Int, end: Int) => {
      for (id <- start to end) {
        val newPost = new Post(id, b64(f("m")), None, None, 0, None).toJson
        val future = IO(Http)(context.system).ask(HttpRequest(POST, Uri(s"$url/users/$uid/post"), entity = HttpEntity(MediaTypes.`application/json`, newPost.toString))).mapTo[HttpResponse]
        val post = Await.result(future, timeout.duration).asInstanceOf[HttpResponse]

        if ((post.status).toString() == "201 Created" || (post.status).toString() == "200 OK") {
          // log.info("id: " + id.toString + " uid -" + uid.toString() + " 201 post")
        } else {
          log.info("id: " + id.toString + " uid -" + uid.toString() + "post status: " + post.status)
        }
      }
      sender ! postsCreationdone() //same msg from 
    }
    case createPostForPage(uid: Int, pid: Int, start: Int, end: Int) => {
      for (id <- start to end) {

        val newPost = new Post(id, b64(f("m")), None, None, 0, None).toJson
        val future = IO(Http)(context.system).ask(HttpRequest(POST, Uri(s"$url/users/$uid/pages/$pid/post"), entity = HttpEntity(MediaTypes.`application/json`, newPost.toString))).mapTo[HttpResponse]
        val postpage = Await.result(future, timeout.duration).asInstanceOf[HttpResponse]
        //log.info("postonpage-" + (postpage.status).toString())
        if ((postpage.status).toString() == "201 Created" || (postpage.status).toString() == "200 OK") {
          // log.info("id: " + id.toString + " uid -" + uid.toString() + " pid: " + pid + " 201 post for page")
        } else {
          log.info("id: " + id.toString + " uid -" + uid.toString() + " pid: " + pid + "post status for page: " + postpage.status)
        }
      }
      sender ! postsCreationdone()
    }
    case createFriendshipNetwork(start: Int, end: Int, friendshipDegree: Int) => {
      for (id <- start to end) {

        for (fid <- 1 to 3) {
          var k: Int = id + fid * friendshipDegree
          val newUser = new User(k, None, None, List(), List(), List(), null).toJson
          val future = IO(Http)(context.system).ask(HttpRequest(POST, Uri(s"$url/users/$id/add_friend"), entity = HttpEntity(MediaTypes.`application/json`, newUser.toString))).mapTo[HttpResponse]
          val response = Await.result(future, timeout.duration).asInstanceOf[HttpResponse]

          if ((response.status).toString() == "201 Created" || (response.status).toString() == "200 OK") {
            //log.info("id: " + id.toString + " fid -" + (fid*end).toString() + " 201  for add_friend")
          } else {
            log.info("id: " + id.toString + " fid -" + (k).toString() + " " + response.status)
          }
        }
      }
      sender ! createFriendshipdone()
    }
  }

}
object UserClientApp extends App {

  println("-- fb model simulator--")

  println("Enter the # of workers to simulate>")
  val workerCount: Int = StdIn.readInt()
  println("Enter the # of users to simulate>")
  val countUsers: Int = StdIn.readInt()

  println("Enter the # of pages to create out of theses users>")
  val countPages: Int = StdIn.readInt()

  println("Enter the # of posts to be made by each user on his feed or on pages>")
  val countPosts: Int = StdIn.readInt()

  implicit val system = ActorSystem("Client")
  //val workerCount: Int = 20
  /*  val countUsers: Int = 100000
  val countPosts: Int = 100000
  val countPages: Int = 100000*/

  val master = system.actorOf(Props(new Master(workerCount, countUsers, countPosts / 2, countPages)), name = "Master")
  master ! usersCreatework()
  //system.shutdown()
}