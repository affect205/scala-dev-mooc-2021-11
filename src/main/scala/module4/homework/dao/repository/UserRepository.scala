package module4.homework.dao.repository

import io.getquill.context.ZioJdbc.QIO
import module4.homework.dao.entity._
import module4.phoneBook.db
import zio.{Has, ULayer, ZLayer}


object UserRepository{


    val dc = db.Ctx
    import dc._

    type UserRepository = Has[Service]

    trait Service{
        def findUser(userId: UserId): QIO[Option[User]]
        def createUser(user: User): QIO[Unit]
        def createUsers(users: List[User]): QIO[Unit]
        def updateUser(user: User): QIO[Unit]
        def deleteUser(user: User): QIO[Unit]
        def findByLastName(lastName: String): QIO[List[User]]
        def list(): QIO[List[User]]
        def userRoles(userId: UserId): QIO[List[Role]]
        def insertRoleToUser(roleCode: RoleCode, userId: UserId): QIO[Unit]
        def listUsersWithRole(roleCode: RoleCode): QIO[List[User]]
        def findRoleByCode(roleCode: RoleCode): QIO[Option[Role]]
    }

    class ServiceImpl extends Service{

        lazy val userSchema = quote {
            querySchema[User](""""User"""")
        }

        lazy val roleSchema = quote {
            querySchema[Role](""""Role"""")
        }

        lazy val userToRoleSchema = quote {
            querySchema[UserToRole](""""UserToRole"""")
        }

        def findUser(userId: UserId): Result[Option[User]] =
            run(userSchema.filter(_.id == lift(userId.id))).map(_.headOption)

        def createUser(user: User): Result[Unit] = {
            run(userSchema.insert(lift(user))).map(_ => ())
        }

        def createUsers(users: List[User]): Result[Unit] = {
            run(liftQuery(users).foreach(u => query[User].insert(u))).map(_ => ())
        }

        def updateUser(user: User): Result[Unit] =
            run(userSchema.update(lift(user))).map(_ => ())

        def deleteUser(user: User): Result[Unit] =
            run(userSchema.filter(_.id == lift(user.id)).delete).map(_ => ())
        
        def findByLastName(lastName: String): Result[List[User]] =
            run(userSchema.filter(_.lastName == lift(lastName)))
        
        def list(): Result[List[User]] =
            run(userSchema)

        def userRoles(userId: UserId): Result[List[Role]] =
            run(userSchema
              .filter(_.id == lift(userId.id))
              .join(userToRoleSchema)
              .on(_.id == _.userId)
              .join(roleSchema)
              .on(_._2.roleId == _.code)
              .map(_._2)
            )

        def insertRoleToUser(roleCode: RoleCode, userId: UserId): Result[Unit] =
            run(userToRoleSchema.insert(lift(UserToRole(roleCode.code, userId.id)))).map(_ => ())
        
        def listUsersWithRole(roleCode: RoleCode): Result[List[User]] =
            run(roleSchema
              .filter(_.code == lift(roleCode.code))
              .join(userToRoleSchema)
              .on(_.code == _.roleId)
              .join(userSchema)
              .on(_._2.userId == _.id)
              .map(_._2)
            )

        def findRoleByCode(roleCode: RoleCode): Result[Option[Role]] =
            run(roleSchema.filter(_.code == lift(roleCode.code))).map(_.headOption)
    }

    val live: ULayer[UserRepository] = ZLayer.succeed(new ServiceImpl)
}
