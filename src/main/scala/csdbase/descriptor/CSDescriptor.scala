package csdbase.descriptor

import java.net.URL

import io.circe._

sealed case class UserGroup(
  user: String,
  group: String)

sealed case class CDHVersion(
  min: Option[String],
  max: Option[String])

sealed case class Compatability(
  generation: Int,
  cdhVersion: Option[CDHVersion])

sealed case class Parcel(
  repoUrl: Option[URL],
  requiredTags: Seq[String],
  optionalTags: Seq[String])

sealed case class ServiceDependency(name: String, required: Option[Boolean])

sealed case class ServiceCommandRef(commandName: String, failureAllowed: Option[Boolean] = None)

sealed case class ServiceCommand(
  name: String,
  label: String,
  description: String,
  roleCommand: String,
  roleName: String,
  runMode: String)

sealed case class ScriptRunner(program: String, args: Option[Seq[String]], environmentVariables: Option[Map[String, String]])

sealed case class StartStopRunner(
  relevantRoleTypes: Option[Seq[String]],
  runner: ScriptRunner,
  timeout: Option[Long],
  masterRole: Option[String])

sealed case class HDFSDir(
  name: String,
  label: String,
  description: String,
  directoryDescription: String,
  path: String,
  permissions: String)

sealed case class CSDescriptorData(
  name: String,
  version: String,
  description: String,
  runAs: UserGroup,
  maxInstances: Option[Int] = None,
  icon: Option[String] = None,
  compatability: Option[Compatability] = None,
  parcel: Option[Parcel] = None,
  serviceDependencies: Option[Seq[ServiceDependency]] = None,
  serviceInit: Option[Seq[ServiceCommandRef]],
  inExpressWizard: Option[Boolean],
  rolesWithExternalLinks: Option[Seq[String]],
  hdfsDirs: Option[Seq[HDFSDir]],
  commands: Option[Seq[ServiceCommand]]
)
