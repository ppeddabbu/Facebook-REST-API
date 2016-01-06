
import scala.util.Random
import java.security.SecureRandom
  import java.security._
  import javax.crypto._
  import org.apache.commons.codec.binary.Base64
  import javax.crypto.spec.SecretKeySpec
  import javax.crypto.Cipher
package fbcrypto {



 class aesCrypto(algoName: String) 
 {
      val ISO_CONST:String="ISO-8859-1"
      
    def encrypt(bytes: Array[Byte], secret: Array[Byte]): String = {
      val sKey       = new SecretKeySpec(secret, algoName)
      val en_cipher  = Cipher.getInstance(algoName)
        // "/ECB/PKCS5Padding"
      en_cipher.init(Cipher.ENCRYPT_MODE, sKey)
      var x=en_cipher.doFinal(bytes)
      new String(x,ISO_CONST)
    }
    
    def decrypt(bytes: Array[Byte], secret: Array[Byte]): String = {
      val sKey       = new SecretKeySpec(secret, algoName)
      val de_cipher  = Cipher.getInstance(algoName)
      
      de_cipher.init(Cipher.DECRYPT_MODE, sKey)
      var y=de_cipher.doFinal(bytes)
         new String(y,ISO_CONST)
    }
  }
  object AES extends aesCrypto("AES")

  class rsaCrypto()
  {
          val ISO_CONST:String="ISO-8859-1"
    var publicKey:PublicKey =null
    var privateKey: PrivateKey=null 

    def genKeys()  {
      val kpg = KeyPairGenerator.getInstance("RSA")
      kpg.initialize(1024)
      val keypair = kpg.generateKeyPair()
      publicKey  =keypair.getPublic()
      privateKey =keypair.getPrivate()
    }
    def getPublicKey(): PublicKey = {
      this.publicKey
    }
    def getPrivateKey(): PrivateKey = {
     this.privateKey
    }

    def encrypt(key: PublicKey, data: Array[Byte]): String =  { 
      val cipher = Cipher.getInstance("RSA")
      cipher.init(Cipher.ENCRYPT_MODE, key)
      var encryptData=cipher.doFinal(data)
      new String(encryptData,ISO_CONST)
    }

    def decrypt(key: PrivateKey, data: String): String = { 
      val cipher = Cipher.getInstance("RSA")
      cipher.init(Cipher.DECRYPT_MODE, key)
      var decryptData=cipher.doFinal(data.getBytes(ISO_CONST))
      new String(decryptData,ISO_CONST)
    }
  }

  object RSA extends rsaCrypto()
}

object Test{
    val ISO_CONST:String="ISO-8859-1"
    import fbcrypto._
    
    var aeskey:Array[Byte]=null
    //var aeskey:String="0123456789012345"
    def GenAESKey():Array[Byte]={
    var rand = new SecureRandom()
    var aesKeye: Array[Byte]= new Array[Byte](16)
    rand.nextBytes(aesKeye)
    aesKeye
    }
    
    def f(a:Any):String = {
      //AES.encrypt(a.toString.getBytes(ISO_CONST),aeskey.getBytes(ISO_CONST))
      AES.encrypt(a.toString.getBytes(ISO_CONST),aeskey)
    }
    
    def g(key: Array[Byte]):String = {
       RSA.encrypt(RSA.getPublicKey(), key) //rsa encryption of aeskey
    }
    
    def h(key:String):String = {
      
      RSA.decrypt(RSA.getPrivateKey(),key) //rsa decryption of aeskey

    }
   
    def i(b: String, aesKeyd:String):String= {
        AES.decrypt(b.getBytes(ISO_CONST),aesKeyd.getBytes(ISO_CONST))    //b.getBytes(ISO_CONST), aesKeyd)
    }
    
   def main(args: Array[String]) = {
    val ISO_CONST:String="ISO-8859-1"    
    for( j <- 1 until 5)
    {
     var input = "The Far Eastern Party uisahfsdhfasdhg"+j.toString()
        
          aeskey=GenAESKey()
          RSA.genKeys()
          
          System.out.println(" original input before decrypt: " + input)
          var e_i:String=f(input)
          System.out.println("encrypted input: " + e_i)
          
          System.out.println(" original aes key before decrypt: " + new String(aeskey,ISO_CONST))
          //var e_aes:String=g(aeskey.getBytes(ISO_CONST))
          var e_aes:String=g(aeskey)
          System.out.println("encrypted aes: " + e_aes)
          
          var d_aes:String=h(e_aes)
          System.out.println("decrypted aes: " + d_aes)
          
          var d_o=i(e_i,d_aes)
          System.out.println("decrypted output: " + d_o)
    }
  }
  
  
}