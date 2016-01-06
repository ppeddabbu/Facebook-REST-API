import spray.json._

object ClientDesign {

  case object Created
  case object AlreadyExists
  case object NotFound
  case object Updated
  case object Deleted
  case class Pic(
    id:      Int,
    desc:    Option[String],
    image:   String)

  case class Album(
    id:         Int,
    caption:    Option[String],
    pics:       List[Pic])

  case class User(
    id:             Int,
    username:       Option[String],
    name:           Option[String],
    /*first_name:     Option[String],
    middle_name:    Option[String],
    last_name:      Option[String],
    email:          Option[String],
    link:           Option[String],
    gender:         Option[String],*/
    page_list:      List[Int],
    friends_list:   List[Int],
    post_list:      List[Int], 
/*    album_list:     Option[List[Album]],
    profilepicture: Option[Pic],*/
    key: String )

  case class Page(
    id:             Int,
    name:           Option[String],
    count_likes:    Int,
    user_list:      List[Int],
    post_list:      List[Int] )

  case class Post(
    id:            Int,
    message:       String,
    postedBy:      Option[Int],
    posted_inpage: Option[Int],
    likes:         Int,
    comments:      Option[List[String]])

  object Pic extends DefaultJsonProtocol {
    implicit val format = jsonFormat3(Pic.apply)
  }
  object Album extends DefaultJsonProtocol {
    implicit val format = jsonFormat3(Album.apply)
  }
  object User extends DefaultJsonProtocol {
    implicit val format = jsonFormat7(User.apply)
  }

  object Page extends DefaultJsonProtocol {
    implicit val format = jsonFormat5(Page.apply)
  }

  object Post extends DefaultJsonProtocol {
    implicit val format = jsonFormat6(Post.apply)
  }

}