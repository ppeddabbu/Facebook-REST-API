package main.scala

import com.typesafe.config.ConfigFactory
import akka.actor._
import akka.io.IO
import akka.pattern.ask
import akka.util.Timeout

import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.collection.mutable.ListBuffer

import spray.can.Http
import spray.httpx.SprayJsonSupport._
import spray.routing._
import spray.http.StatusCodes

import FbDesign._

object FbApi extends App{
  
  //def  main(args: Array[String]) {
    

  implicit  val actorSystem= ActorSystem("facebookAPImodel")
  //implicit val executionContext = actorSystem.dispatcher
  val fbApiHandler=actorSystem.actorOf(Props(new FbHTTPRest()), "FbHTTPRestInterface")
 
  val appConf = ConfigFactory.load()
  val ip = appConf.getString("http.ipaddress")
  val portNo = appConf.getInt("http.portno")
  
  implicit val timeOut= Timeout(15 seconds)
  val future= IO(Http)?(Http.Bind(fbApiHandler,interface=ip,port=portNo))
  
    future.mapTo[Http.Event].map{
    case Http.Connected => println("bind successful")
    case Http.Bound(address) => println(s"fb REST interface listens at $address")
    case Http.Aborted =>println("http connection aborted!")
    case Http.CommandFailed(msg) => println(s"bind command failed, not able to bound to $ip:$portNo; failure msg- ${msg.failureMessage}")
  }
 //}
}

trait FbRestRouteApi extends HttpService with ActorLogging { a:Actor=>
  val users = ListBuffer[User]()
  val pages = ListBuffer[Page]()
  val posts = ListBuffer[Post]()
  
  def httpRoutes: Route=
 /*    pathPrefix("get_pages") {
      pathEnd {
        get {
          entity(as[Page]) { page => reqCtx =>
            val replyActor = createReplyActor(reqCtx)
            replyActor ! pages
          }
        }
      } 
    }~*/
    pathPrefix("pages" / IntNumber){ pgid=>
      pathEnd {
        get { reqCtx =>
          val replyActor = createReplyActor(reqCtx)
          getPage(pgid).map(replyActor ! _)
            .getOrElse(replyActor ! NotFound)
        }
      }
    } ~
    pathPrefix("user") {
      pathEnd {
        put {
          entity(as[User]) { user => reqCtx =>
            val replyActor = createReplyActor(reqCtx)
            createUser(user) match {
              case true => replyActor ! Created
              case _ => replyActor ! AlreadyExists
            }
          }
        }
      } 
    }~
    pathPrefix("users"/IntNumber){ uid =>
      pathEnd {
        get { reqCtx =>
          val replyActor = createReplyActor(reqCtx)
          getUser(uid).map(replyActor ! _)
            .getOrElse(replyActor ! NotFound)
        } ~
        post{ entity(as[User]){ user =>reqCtx=>
          val replyActor = createReplyActor(reqCtx)
          updateUser(user,uid)
          replyActor ! Updated
         }
        } ~
        delete { reqCtx =>
          val replyActor = createReplyActor(reqCtx)
          deleteUser(uid)
          replyActor ! Deleted
        } 
      } ~
      path("page") {
      pathEnd {
        put {
          entity(as[Page]) { page => reqCtx =>
            val replyActor = createReplyActor(reqCtx)
            createPage(page,uid) match {
              case true => replyActor ! Created
              case _ => replyActor ! AlreadyExists
            }
          }
        }
      } 
    }~ 
    path("pages" / IntNumber){ pgid=>
      pathEnd {
        get { reqCtx =>
          val replyActor = createReplyActor(reqCtx)
          getPage(pgid).map(replyActor ! _)
            .getOrElse(replyActor ! NotFound)
        } ~
        delete { reqCtx=>
          val replyActor = createReplyActor(reqCtx)
          deletePage(pgid)
          replyActor ! Deleted
        } ~
        post{ entity(as[Page]){ page =>reqCtx=>
          val replyActor = createReplyActor(reqCtx)
          updatePage(page, pgid, 0 , true)
          replyActor ! Updated
         }
        }
      } ~
      path("get_all_posts"){
        pathEnd{
        get { reqCtx =>
          val replyActor = createReplyActor(reqCtx)
          //has to retreive the particular post matched by uid?? is this right?
          getAllPostsByPage(pgid).map(replyActor ! _)
        } 
      }
    }
    } ~
      path("get_all_posts"){
        pathEnd{
        get { reqCtx =>
          val replyActor = createReplyActor(reqCtx)
          //has to retreive the particular post matched by uid?? is this right?
          //replyActor ! posts
          getAllPostsByUser(uid).map(replyActor ! _)
        } 
      }
      }~
      path("post"){
        post{
         entity(as[Post]){ post=> reqCtx=>
              val replyActor = createReplyActor(reqCtx)
              // create post is it required? yes does the update as well 
              createPost(uid,-1,post) match{
                case true => replyActor! Created
                case _ => replyActor! AlreadyExists
           }
         }
        }
      }~
      path("posts"/IntNumber){  psid =>
       get { reqCtx =>
          val replyActor = createReplyActor(reqCtx)
          //has to retreive the particular post matched by
          getPostByUser(uid, psid).map(replyActor ! _)
            .getOrElse(replyActor ! NotFound)
        } ~
       delete { reqCtx=>
          val replyActor = createReplyActor(reqCtx)
          deletePostByUser(uid, psid)
          replyActor ! Deleted
        } 
       }~
	   path("pages"/IntNumber/"post"){pgid=>
       post{
         entity(as[Post]){
            post=> reqCtx=>
              val replyActor = createReplyActor(reqCtx)
              // create post is it required? post will be either part of page or user 
              createPost(uid,pgid,post) match{
                case true => replyActor! Created
                case _ => replyActor! AlreadyExists
           }
         }
       }
	   } ~
	   path("add_profile_pic"){
	     post{ entity(as[Pic]){ pic =>reqCtx=>
          val replyActor = createReplyActor(reqCtx)
          addProfilePicToUser(uid,pic)
          replyActor ! Updated
         }
        } 
	   } ~
	   path("albums"/ IntNumber){albid =>
	     pathEnd{
	       get{reqCtx =>
          val replyActor = createReplyActor(reqCtx)
          //has to retreive the particular post matched by
          getAlbumByUser(uid,albid).map(replyActor ! _)
           .getOrElse(replyActor ! NotFound)
	      }
	     }
	   }~
	   path("albums"/ IntNumber/"add_pic"){albid =>
	       post{ entity(as[Pic]){ pic =>reqCtx=>
          val replyActor = createReplyActor(reqCtx)
          addPicToUserAlbum(uid,albid, pic)
          replyActor ! Updated
         }
        }
	   } ~
	   path("add_album"){
	     post{ entity(as[Album]){ picList =>reqCtx=>
          val replyActor = createReplyActor(reqCtx)
          addAlbumToUser(uid,picList)
          replyActor ! Updated
         }
        } 
	   } ~
	    path("remove_friend"){ 
        post { entity(as[User]){ user => reqCtx=>
          val replyActor = createReplyActor(reqCtx)
          friendShip(uid, user.id , false)             // false to remove friend
          replyActor ! Updated
          }
        }
      } ~
      path("add_friend"){ 
        post { entity(as[User]){ user => reqCtx=>
          val replyActor = createReplyActor(reqCtx)
          friendShip(uid, user.id, true)               // true to add friend
          replyActor ! Updated
          }
        }
      } ~
      path("like_page"){ 
        post { entity(as[Page]){ page => reqCtx=>
          val replyActor = createReplyActor(reqCtx)
          updatePage(page,page.id, uid, true)
          replyActor ! Updated
          }
        }
      } ~
      path("unlike_page"){ 
        post { entity(as[Page]){ page => reqCtx=>
          val replyActor = createReplyActor(reqCtx)
          updatePage(page,page.id, uid , false)
          replyActor ! Updated
          }
        }
      } 
    }
    
  private def addProfilePicToUser(uid:Int, pic:Pic)={
    var q1:User = users.find(_.id == uid).get.asInstanceOf[User]
    var index: Int=users.indexWhere(_.id == uid)

    //need to add this to album as well
    var pl=Album(id=1234,Some("profile pictures"),pics=List(pic))
    var albm:Option[List[Album]]=q1.album_list
    if(albm!=None){
     var album:List[Album]=albm.get.asInstanceOf[List[Album]]
     album=pl::album
     q1=q1.copy(profilepicture=Some(pic),album_list=Some(album))
    }
    else{
     q1=q1.copy(profilepicture=Some(pic),album_list = Some(List(pl)))
    }
      
    users.update(index, q1)
  }
    private def getAlbumByUser(uid:Int, albid:Int): Option[Album]={
      var q1:User = users.find(_.id == uid).get.asInstanceOf[User]
      q1.album_list.get.find(_.id ==albid)
    }
   private def addPicToUserAlbum(uid:Int,albid:Int, pic:Pic)={
    var q1:User = users.find(_.id == uid).get.asInstanceOf[User]
    var index: Int=users.indexWhere(_.id == uid)
    
    var albmList:Option[List[Album]]=q1.album_list
     if(albmList!=None){
     var album:Album=albmList.get.find(_.id == albid).get.asInstanceOf[Album]
     var albumPics: List[Pic]=album.pics
     albumPics =pic ::albumPics
     album = album.copy(pics=albumPics)
     var albIndex=albmList.get.indexWhere(_.id == albid)
     albmList=Some(albmList.get.updated(albIndex, album))
     q1=q1.copy(album_list = albmList)
    }
    
    users.update(index, q1)
   }
    private def addAlbumToUser(uid:Int, picList:Album)={
    var q1:User = users.find(_.id == uid).get.asInstanceOf[User]
    var index: Int=users.indexWhere(_.id == uid)
    
    var albm:Option[List[Album]]=q1.album_list
     if(albm!=None){
     var album:List[Album]=albm.get.asInstanceOf[List[Album]]
     album=picList::album
     q1=q1.copy(album_list=Some(album))
    }
    else{
      q1=q1.copy(album_list = Some(List(picList)))
    }
    
    users.update(index, q1)
  }
    
  private def friendShip(id1: Int, id2:Int, friendship:Boolean) = {
    var q1:User = users.find(_.id == id1).get.asInstanceOf[User]
    var index1:Int = users.indexWhere(_.id == id1)

    var q2:User = users.find(_.id == id2).get.asInstanceOf[User]
    var index2:Int = users.indexWhere(_.id == id2)

    var friends_list1:List[Int] = q1.friends_list
    var friends_list2:List[Int] = q2.friends_list

    if(friendship){
      friends_list1 = id2 :: friends_list1
      friends_list2 = id1 :: friends_list2
    } else{
      friends_list1 = friends_list1.filter(_ != id2)
      friends_list2 = friends_list1.filter(_ != id1)
    }

    q1 = q1.copy(friends_list = friends_list1)
    users.update(index1, q1)
    q2 = q2.copy(friends_list = friends_list2)
    users.update(index2, q2)
  }

  private def createUser(user: User): Boolean = {
    val doesNotExist = !users.exists(_.id == user.id)
    
    if (doesNotExist) {
      users.append(user)
      log.info(users.toString)
    }
    doesNotExist
  }
  private def getUser(id: Int): Option[User] = {
    users.find(_.id == id)
  }
  private def updateUser(user:User, id:Int) ={
   var q:User = users.find(_.id == id).get.asInstanceOf[User]
    var index:Int = users.indexWhere(_.id == id)

    var first_name:Option[String] = user.first_name
    if(first_name==""){
      first_name = q.first_name
    }
    var middle_name:Option[String] = user.middle_name
    if(middle_name==""){
      middle_name = q.middle_name
    }
    var last_name:Option[String] = user.last_name
    if(last_name==""){
      last_name = q.last_name
    }
    var username:Option[String] = user.username
    if(username==""){
      username = q.username
    }
    var link:Option[String] = user.link
    if(link==""){
      link = q.link
    }
    var email:Option[String] = user.email
    if(email==""){
      email = q.email
    }
    var gender:Option[String] = user.gender
    if(gender==""){
      gender = q.gender
    }
    
    q = q.copy(first_name = first_name , middle_name = middle_name, last_name = last_name, username = username, link = link, email = email, gender = gender)
    users.update(index, q)
  }
    private def deleteUser(id: Int): Unit = {
    var n:Int=users.indexWhere(_.id == id)
    if(n>0)
    users.remove(n)
  }
  private def createPage(page: Page, uid:Int): Boolean = {
    val bool = !pages.exists(_.id == page.id)
    var p:Page=page
    if (bool){
      p=p.copy(admin = Some(uid))
      pages.append(p)
    }
    bool
  }

  private def updatePage(page:Page, id: Int, userId:Int, include:Boolean): Unit = {

    var qUser:User = users.find(_.id == userId).get.asInstanceOf[User]
    var indexUser:Int = users.indexWhere(_.id == userId)

    var q:Page  = pages.find(_.id == id).get.asInstanceOf[Page]
    var index:Int = pages.indexWhere(_.id == id)
//if index -1, that is no element results in exception, are we going into that scenario?
    var likes:Int = q.count_likes
    var name:Option[String] = page.name
    if(page.name==""){
      name = q.name
    }
    var user_list:List[Int] = q.user_list
    var page_list:List[Int] = qUser.page_list

    if(userId!=0){
      if(include && !user_list.contains(userId)){
        likes = likes + 1
        user_list = userId :: user_list
        page_list = id :: page_list
      }
      if(!include && user_list.contains(userId)){
        likes = likes - 1
        user_list = user_list.filter(_ != userId)
        page_list = page_list.filter(_ != id)
      }
        qUser = qUser.copy(page_list = page_list)
        users.update(indexUser, qUser)
    }
    q = q.copy(name = name, count_likes = likes , user_list = user_list)
    pages.update(index, q) 
  }
  private def deletePage(id: Int): Unit = {
    var n:Int=pages.indexWhere(_.id == id)
    if(n>0)
    pages.remove(n)
  }
  private def getPage(id: Int): Option[Page] = {
    pages.find(_.id == id)
  }
    private def createPost(uid: Int, pgid: Int, post: Post): Boolean = {
    var p:Post=post
    p = p.copy(postedBy = Some(uid), posted_inpage = Some(pgid))
    posts.append(p)
    
    var psid:Int=post.id
    //log.info(" uid- "+uid+" pgid -"+pgid+" psid -"+psid)
    //log.info("userslist "+users.toString())
    var user: User = users.find(_.id == uid).get.asInstanceOf[User]
    var userIndex: Int = users.indexWhere(_.id == uid)
    if (userIndex >= 0) {
        //log.info(" user index ...uid- "+uid+" pgid -"+pgid+" psid -"+psid)
      var userPostList: List[Int] = user.post_list
      userPostList = psid :: userPostList
      user = user.copy(post_list = userPostList)
      users.update(userIndex, user)
    }

    if (pgid > 0) {
        //log.info(" pgidIndex.. uid- "+uid+" pgid -"+pgid+" psid -"+psid)
      var page: Page = pages.find(_.id == pgid).get.asInstanceOf[Page]
      var pageIndex: Int = pages.indexWhere(_.id == pgid)
      if (pageIndex >= 0) {
        var pagePostList: List[Int] = page.post_list
        pagePostList = psid :: pagePostList
        page = page.copy(post_list = pagePostList)
        pages.update(pageIndex, page)
      }
    }
    true
  }
    private def getPostByUser(uid: Int, psid: Int): Option[Post] = {
      posts.filter(_.postedBy==Some(uid)).find(_.id == psid)
      
  }
    private def getAllPostsByUser(uid: Int): String = {
      Some(posts.filter(_.postedBy==Some(uid))).toString()
  }
    private def deletePostByUser(uid:Int, psid: Int): Unit = {
    //check
    var indexUser:Int = posts.filter(_.postedBy==Some(uid)).indexWhere(_.id == uid)
    posts.remove(indexUser)
  }
    private def getAllPostsByPage( pgid: Int): String = {
      Some(posts.filter(_.posted_inpage==Some(pgid))).toString()
  }
       
  private def createReplyActor(reqCtx:RequestContext)={
      context.actorOf(Props(new ReplyActor(reqCtx)))
    }
}

class FbHTTPRest extends HttpServiceActor with FbRestRouteApi{
  def receive =runRoute(httpRoutes)
}

class ReplyActor(ctx:RequestContext) extends HttpServiceActor{
  private def done =self ! PoisonPill

  def receive = {
    case Created =>
      ctx.complete(StatusCodes.Created)
      done

    case AlreadyExists =>
      ctx.complete(StatusCodes.Conflict)
      done
    case NotFound =>
      ctx.complete(StatusCodes.NotFound)
      done

    case Updated =>
      ctx.complete(StatusCodes.OK)
      done

    case Deleted =>
      ctx.complete(StatusCodes.OK)
      done
    case user: User =>
      ctx.complete(StatusCodes.OK, user)
      done

    case page: Page =>
      ctx.complete(StatusCodes.OK, page)
      done

    case post: Post =>
      ctx.complete(StatusCodes.OK, post)
      done 
      
    case album: Album =>
      ctx.complete(StatusCodes.OK, album)
      done
     
    case pic: Pic =>
      ctx.complete(StatusCodes.OK, pic)
      done
    case s:String =>
      ctx.complete(StatusCodes.OK, s)
      done
  }
}