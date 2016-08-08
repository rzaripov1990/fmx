**************************************************************
Author: TMS Software
        Copyright © 2015
        E-mail: info@tmssoftware.com
        Web: http://www.tmssoftware.com
**************************************************************


iOS9 fix (RAD Studio 10 Seattle)
--------------------------------------------------------------

Update: You now optionally add additional keys in the iOS9Fix_config.txt file

Linking against iOS 9 introduces the new Application Transport Security feature that enforces a secure network connection. This means that in a default RAD Studio 10 Seattle project, the TWebBrowser but also our TTMSFMXWebBrowser, TTMSFMXWebGMaps, TTMSFMXWebOSMaps components and the TMS Cloud Pack for FireMonkey cloud services can no longer make a connection to the web. A solution for this issue is to add the following key to the Info.plist file:

<key>NSAppTransportSecurity</key>
<dict>
  <key>NSAllowsArbitraryLoads</key>
      <true/>
</dict>

As the Info.plist is automatically generated based on the project options, there is no option to specify a boolean key in a dictionary. To make sure the Info.plist file contains the correct keys in order to link against iOS 9 we have created a iOS9Fix command line executable and a post-build command.

1) Copy the iOS9Fix.exe & iOS9Fix_config.txt files to the project directory. (iOS9Fix1.png)
2) Add the following post-build command to your project options. (iOS9Fix2.png)
   
call "$(PROJECTDIR)\iOS9Fix.exe" "$(OUTPUTPATH).info.plist"

