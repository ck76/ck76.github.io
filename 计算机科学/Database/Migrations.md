[TOC]


- https://roadmap.sh/postgresql-dba

# Migrations

Migrations are a way to manage and evolve your database schema over  time. As your application grows and its requirements change, you’ll need to modify the database schema to accommodate new features or  enhancements. In PostgreSQL, migrations allow for a structured and  version-controlled way to apply these changes incrementally, making it  easier to develop, test, and collaborate on database schema updates.

## Key Concepts

- **Migration**: A migration is a single unit of change  that affects the schema or data in a database. Each migration  encapsulates an operation such as creating, altering, or dropping  tables, indices, or constraints.
- **Migration History**: The sequence of applied  migrations is the migration history, and it helps you keep track of the  transformations applied to the schema over time. Typically, migrations  are tracked using a dedicated table in the database that logs applied  migrations and their order.
- **Up and Down Migrations**: Each migration typically  consists of two operations – an “up” operation that applies the change,  and a “down” operation that rolls back the change if needed. The up  operation moves the schema forward, while the down operation reverts it.

## Benefits of Migrations

- **Version Control**: Migrations help to version control your database schema, making it easier to collaborate with team members and review schema changes in the same way you review application code.
- **Consistency**: Migrations promote a consistent and  reproducible approach to managing schema changes across various  environments (e.g., development, testing, production).
- **Testability**: Migrations allow you to test the effect of schema changes in isolated environments before deploying them to production.
- **Deployability**: Migrations facilitate automated deployment processes and help reduce the risk of human error during database schema updates.

## Migration Tools

Several tools are available that support migrations in PostgreSQL, including:

- [Alembic](https://alembic.sqlalchemy.org/en/latest/): A lightweight and extensible migration tool written in Python that  works seamlessly with SQLAlchemy (a popular ORM for Python).
- [Flyway](https://flywaydb.org/): A popular Java-based database migration tool that supports PostgreSQL, among other databases.
- [Liquibase](https://www.liquibase.org): An open-source, Java-based database migration tool that supports multiple databases including PostgreSQL.
- [Node-pg-migrate](https://github.com/salsita/node-pg-migrate): A convenient migration tool for Node.js applications that use PostgreSQL as their back-end.

To effectively leverage migrations for your PostgreSQL application,  you should choose a migration tool that fits the technology stack and  workflow of your team. Once you have selected a tool, start  incorporating migrations into your application’s development and  deployment processes, ensuring consistency, testability, and easier  collaboration on schema updates.



----

# Practical Patterns for Migrations

In this section, we’ll discuss some practical patterns and strategies that you can implement while working with migrations in PostgreSQL.  These tips are invaluable for keeping your database schema up-to-date  and maintaining a seamless development process across multiple  environments.

## Migration Naming Conventions

Choose a consistent naming convention for your migration files. Typically, the preferred format is `<timestamp>_<short_description>.sql`. This ensures that migrations are ordered chronologically and can be easily identified.

Example: `20210615_create_users_table.sql`

## Apply One Change per Migration

To keep your migrations clean and easy to understand, apply only one  schema change per migration file. This way, developers can easily figure out what changes have been applied and in what order.

Example:

- `20210615_create_users_table.sql`
- `20210616_add_email_to_users.sql`

## Use Idempotent SQL to Rollback

When working with databases, it’s only a matter of time before you might need to rollback a change. Ensure that each `UP` migration script has a corresponding `DOWN` migration script to revert changes.

Example: In `20210616_add_email_to_users.sql`:

```
-- UP
ALTER TABLE users ADD COLUMN email TEXT NOT NULL;

-- DOWN
ALTER TABLE users DROP COLUMN email;
```

## Test Migrations Thoroughly

Always test your migrations thoroughly, both up and down, before  applying them to a production environment. It’s essential to catch  errors in the migration process before they have lasting effects on your system.

## Use Seed Data & Sample Data

Having seed data and sample data can be helpful to initialize an  empty database and provide a baseline for developers to work with. In  addition to schema migration files, consider including these in your  version control as well.

## Automate Deployment of Migrations

Consider using tools and frameworks to automate the application of  migrations across different environments. This will ensure that your  schema changes are applied consistently, reducing the chances of human  error.

Popular tools for automating PostgreSQL migrations include:

- [Flyway](https://flywaydb.org/)
- [Alembic](https://alembic.sqlalchemy.org/)
- [Sqitch](https://sqitch.org/)

By following these practical patterns, you’ll have a more efficient  and maintainable migration process for your PostgreSQL projects, making  it easier for your team to collaborate and manage schema changes over  time.



---

# liquibase, sqitch, Bytebase, ora2pg etc

Migrations are crucial in the lifecycle of database applications. As  the application evolves, changes to the database schema and sometimes  data itself become necessary. In this section, we will explore four  popular migration tools—Liquibase, Sqitch, Bytebase, and Ora2Pg provide  you with a brief summary of each.

### Liquibase

[Liquibase](https://www.liquibase.org/) is an open-source database-independent library for tracking, managing,  and applying database schema changes. It can be integrated with various  build environments, such as Maven or Gradle, and supports multiple  database management systems, including PostgreSQL.

Liquibase tracks changes in XML, YAML, JSON, or SQL format and  utilizes a changeset to uniquely identify each migration. Some  advantages of Liquibase include its robust support for various database  platforms and its compatibility with version control systems like Git or SVN.

### Sqitch

[Sqitch](https://sqitch.org/) is another database-agnostic schema change management tool. It does not require a specific file format for migration scripts, allowing  developers to work with their preferred language (e.g., PL/pgSQL or  PL/Tcl).

Sqitch stores metadata about changes in a separate schema, which  makes it easy to understand the relationship between changes and their  dependencies. Furthermore, it integrates well with version control  systems, making it a popular choice for managing database migrations.

### Bytebase

[Bytebase](https://bytebase.io/) is a web-based, open-source database schema change management tool that plays well with PostgreSQL. It provides a user-friendly interface for  managing migrations, collaborating with team members, and tracking the  progress of changes across multiple environments.

Bytebase offers features such as schema versioning,  pull-request-style reviews, and automated deployment. Its intuitive  interface and collaborative features make it an excellent choice for  teams with non-technical users or organizations looking for more control over their migration process.

### Ora2Pg

[Ora2Pg](https://ora2pg.darold.net/) is a specific migration tool designed to facilitate the migration of  Oracle database schemas and data to PostgreSQL. It provides support for  various schema objects, including tables, indexes, sequences, views, and more.

Ora2Pg can export schema information in various formats, including  SQL or PL/pgSQL, and generate migration scripts to ease the transition  from Oracle to PostgreSQL. If you’re planning to switch from an Oracle  database to PostgreSQL, Ora2Pg is a valuable tool to streamline the  migration process.

In conclusion, Liquibase, Sqitch, Bytebase, and Ora2Pg are four  powerful migration tools that can help you manage your database schema  changes in a PostgreSQL environment. By understanding each tool’s  capabilities, you can select the right one for your specific needs and  ensure smooth database migrations throughout your application’s  lifecycle.