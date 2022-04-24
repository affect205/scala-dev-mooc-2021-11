package module4.homework.services

import zio.Has
import zio.Task
import module4.homework.dao.entity.{Role, RoleCode, User, UserId, UserToRole}
import module4.homework.dao.repository.UserRepository
import zio.interop.catz._
import zio.ZIO
import zio.RIO
import zio.ZLayer
import zio.macros.accessible
import module4.phoneBook.db

@accessible
object UserService{
    type UserService = Has[Service]

    trait Service{
        def listUsers(): RIO[db.DataSource, List[User]]
        def listUsersDTO(): RIO[db.DataSource, List[UserDTO]]
        def addUserWithRole(user: User, roleCode: RoleCode): RIO[db.DataSource, UserDTO]
        def listUsersWithRole(roleCode: RoleCode): RIO[db.DataSource, List[UserDTO]]
    }

    class Impl(userRepo: UserRepository.Service) extends Service{
        val dc = db.Ctx
        import dc._

        def listUsers(): RIO[db.DataSource, List[User]] =
            userRepo.list()


        def listUsersDTO(): RIO[db.DataSource,List[UserDTO]] = {
            for {
                users <- userRepo.list()
                userDTOs <- toUsersDTO(users)
            } yield userDTOs
        }

        def addUserWithRole(user: User, roleCode: RoleCode): RIO[db.DataSource, UserDTO] = for {
            _ <- db.Ctx.transaction(
                for{
                    _ <- userRepo.createUser(user)
                    _ <- userRepo.insertRoleToUser(roleCode, UserId(user.id))
                } yield ()
            )
            userDTO <- toUserDTO(user)
        } yield userDTO
        
        def listUsersWithRole(roleCode: RoleCode): RIO[db.DataSource,List[UserDTO]] = for {
            users <- userRepo.listUsersWithRole(roleCode)
            usersDTO <- toUsersDTO(users)
        } yield usersDTO

        private def toUserDTO(user: User): RIO[db.DataSource, UserDTO] = for {
            usersDTO <- toUsersDTO(List(user))
        } yield usersDTO.head

        private def toUsersDTO(users: List[User]): RIO[db.DataSource, List[UserDTO]] = for {
            usersDTO <- ZIO.collectAll(users.map(user =>
                userRepo.userRoles(UserId(user.id)).map(roles =>
                    UserDTO(user, roles.toSet))))
        } yield usersDTO
    }

    val live: ZLayer[UserRepository.UserRepository, Nothing, UserService.UserService] = {
        ZLayer.fromService[UserRepository.Service, UserService.Service](userRepo => new Impl(userRepo))
    }
}

case class UserDTO(user: User, roles: Set[Role])