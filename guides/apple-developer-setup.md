# Apple Developer Setup

This guide assumes your company is already set up for Apple development and you are in contact with someone that can invite you as a developer.



1. Get OSX Catalina or higher.

2. Set up an Apple ID for your company email

![Screen Shot 2019-12-20 at 3.36.45 PM](./assets/Screen%20Shot%202019-12-20%20at%203.36.45%20PM.png)

3. Install XCode

![Screen Shot 2019-12-20 at 3.39.49 PM](./assets/Screen%20Shot%202019-12-20%20at%203.39.49%20PM.png)

4. Get the unique id (UDID) for the device you are planning on using and verify that it is part of the company profile. You can get the UDID by connecting the device to the computer and then clinking on the description to reveal the UDID.
   ![Screen Shot 2019-12-20 at 4.01.26 PM](./assets/Screen%20Shot%202019-12-20%20at%204.01.26%20PM.png)

![Screen Shot 2019-12-20 at 4.01.40 PM](./assets/Screen%20Shot%202019-12-20%20at%204.01.40%20PM.png)

5. Now that we know the UDID, let's go to the Apple developer portal
   ![Screen Shot 2019-12-20 at 4.03.20 PM](./assets/Screen%20Shot%202019-12-20%20at%204.03.20%20PM.png)

![Screen Shot 2019-12-20 at 4.04.30 PM](./assets/Screen%20Shot%202019-12-20%20at%204.04.30%20PM.png)

![Screen Shot](./assets/Screen%20Shot.png)

6. Now let's add certificate for our newly created Apple ID:
   Open Keychain Access on the Mac:
   ![Screen Shot 2019-12-20 at 4.46.34 PM](./assets/Screen%20Shot%202019-12-20%20at%204.46.34%20PM.png)

And after clicking on Certificates and System, use the Certificate Assistant to Request a Certificate From a Certificate Authority and Save it to a file:



![Screen Shot 2019-12-20 at 4.47.42 PM](./assets/Screen%20Shot%202019-12-20%20at%204.47.42%20PM.png)

![Screen Shot 2019-12-20 at 4.50.44 PM](./assets/Screen%20Shot%202019-12-20%20at%204.50.44%20PM.png)

And save it on your computer

![Screen Shot 2019-12-20 at 4.50.54 PM](./assets/Screen%20Shot%202019-12-20%20at%204.50.54%20PM.png)

With that file, you can use the apple developer portal to get a iOS App Development certificate

![Screen Shot 2019-12-20 at 4.49.09 PM](./assets/Screen%20Shot%202019-12-20%20at%204.49.09%20PM.png)

![Screen Shot 2019-12-20 at 4.51.18 PM](./assets/Screen%20Shot%202019-12-20%20at%204.51.18%20PM.png)

![Screen Shot 2019-12-20 at 4.51.49 PM](./assets/Screen%20Shot%202019-12-20%20at%204.51.49%20PM.png)

And double click it to install it in the keychain

7. With the device ID and the certificate in the system, you should ask your Apple Development Team administrator to regenerate a Profile that links your new certificate and the device. Once that is available, you can download it via the portal

![Screen Shot 2019-12-20 at 4.56.27 PM](./assets/Screen%20Shot%202019-12-20%20at%204.56.27%20PM.png)

and then double click it to install it:

![Screen Shot 2019-12-20 at 4.56.37 PM](./assets/Screen%20Shot%202019-12-20%20at%204.56.37%20PM.png)

![Screen Shot 2019-12-20 at 4.56.49 PM](./assets/Screen%20Shot%202019-12-20%20at%204.58.54%20PM.png)

8. Check you can see the device from Xcode:
   ![Screen Shot 2019-12-20 at 6.27.31 PM](./assets/Screen%20Shot%202019-12-20%20at%206.27.31%20PM.png)

![Screen Shot 2019-12-20 at 6.28.04 PM](./assets/Screen%20Shot%202019-12-20%20at%206.28.04%20PM.png)
