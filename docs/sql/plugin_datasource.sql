SET NAMES utf8mb4;
SET FOREIGN_KEY_CHECKS = 0;

-- ----------------------------
-- Table structure for plugin_datasource
-- ----------------------------
DROP TABLE IF EXISTS `plugin_datasource`;
CREATE TABLE `plugin_datasource`
(
    `datasource_id`          bigint(20)                                             NOT NULL AUTO_INCREMENT,
    `datasource_code`        varchar(30) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin  NOT NULL COMMENT '数据源编码',
    `datasource_description` varchar(60) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin  NULL     DEFAULT NULL COMMENT '描述',
    `datasource_type`        varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin NOT NULL COMMENT '数据源类型，如RDB/NOSQL/HTTP/MQ等',
    `datasource_class`       varchar(30) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin  NOT NULL COMMENT '具体的数据源类型，如MYSQL/ES/POSTGRESQL/HIVE等',
    `database_pool_type`     varchar(20) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin  NULL     DEFAULT NULL COMMENT '数据库连接池类型，如HIKARI/DRUID',
    `database_pool_setting`  text CHARACTER SET utf8mb4 COLLATE utf8mb4_bin         NULL COMMENT '数据库连接池配置',
    `driver_id`              bigint(20)                                             NULL     DEFAULT NULL COMMENT '数据源驱动ID',
    `settings_info`          text CHARACTER SET utf8mb4 COLLATE utf8mb4_bin         NULL COMMENT '数据源配置',
    `enabled_flag`           tinyint(1)                                             NOT NULL DEFAULT 1 COMMENT '是否启用 1:启用 0：不启用',
    `tenant_id`              bigint(20)                                             NOT NULL DEFAULT 0 COMMENT '租户ID',
    `object_version_number`  bigint(20)                                             NOT NULL DEFAULT 1 COMMENT '行版本号，用来处理锁',
    `creation_date`          datetime(0)                                            NOT NULL DEFAULT CURRENT_TIMESTAMP(0),
    `created_by`             int(11)                                                NOT NULL DEFAULT -1,
    `last_updated_by`        int(11)                                                NOT NULL DEFAULT -1,
    `last_update_date`       datetime(0)                                            NOT NULL DEFAULT CURRENT_TIMESTAMP(0),
    PRIMARY KEY (`datasource_id`) USING BTREE,
    UNIQUE INDEX `plugin_datasource_u1` (`datasource_code`, `tenant_id`) USING BTREE
) ENGINE = InnoDB
  AUTO_INCREMENT = 4
  CHARACTER SET = utf8mb4
  COLLATE = utf8mb4_bin
  ROW_FORMAT = Dynamic;

SET FOREIGN_KEY_CHECKS = 1;
