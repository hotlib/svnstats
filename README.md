# svnstats
Script that parses a verbose svn log. You can produce svn logs via

```sh
svn log -v >> svn.log
```
The script reads from the file *svn.log* and gives results in the form *(username, number_of_commits)*. It also
shows the top 15 most commited java classes in the repository like *(java_class, number_of_commits)*. 

An example of the output:
```sh
user@system:$ runhaskell svnparse.hs
"Weekend commits:"
[("developerX",3), ("developerY",1)]
"Total commits:"
[("developerX",612), ("developerY",486)]
"Commits between 22:00 and 05:00"
[("developerY",31)]
"Top 15 changed java classes: "
("/project/trunk/app/AbstractView.java",83)
("/project/trunk/app/IsModule.java",74)
("/project/trunk/app/DashboardMessages.java",73)
("/project/trunk/app/StatisticsView.java",71)
("/project/trunk/app/UserDaoImpl.java",66)
("/project/trunk/app/AbstractUserPresenter.java",63)
("/project/trunk/app/StatisticsResource.java",63)
("/project/trunk/app/StatisticsPresenter.java",56)
("/project/trunk/app/DashboardModule.java",53)
("/project/trunk/app/PhotoResource.java",49)
("/project/trunk/app/AbstractConfigurator.java",49)
("/project/trunk/app/UserService.java",45)
("/project/trunk/app/UserRegistry.java",45)
("/project/trunk/app/UserWorkflow.java",45)
("/project/trunk/app/Main.java",44)
```
