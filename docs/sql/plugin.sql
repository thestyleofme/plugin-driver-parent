SET NAMES utf8mb4;
SET FOREIGN_KEY_CHECKS = 0;

-- ----------------------------
-- Table structure for plugin
-- ----------------------------
DROP TABLE IF EXISTS `plugin`;
CREATE TABLE `plugin`  (
  `id` bigint(20) NOT NULL AUTO_INCREMENT,
  `plugin_id` varchar(30) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin NOT NULL COMMENT '插件id',
  `plugin_description` varchar(60) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin NULL DEFAULT NULL COMMENT '描述',
  `plugin_version` varchar(15) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin NOT NULL COMMENT '插件版本，如1.0.0',
  `plugin_big_type` varchar(30) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin NOT NULL COMMENT '插件大类',
  `plugin_small_type` varchar(30) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin NOT NULL COMMENT '插件小类',
  `plugin_path` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin NULL DEFAULT NULL COMMENT '插件包路径',
  `object_name` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin NULL DEFAULT NULL COMMENT '插件包在minio上的名称',
  `plugin_fingerprint` varchar(60) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin NULL DEFAULT NULL COMMENT '插件包指纹',
  `enabled_flag` tinyint(1) NOT NULL DEFAULT 1 COMMENT '是否启用 1:启用 0：不启用',
  `tenant_id` bigint(20) NOT NULL DEFAULT 0 COMMENT '租户ID',
  `object_version_number` bigint(20) NOT NULL DEFAULT 1 COMMENT '行版本号，用来处理锁',
  `creation_date` datetime(0) NOT NULL DEFAULT CURRENT_TIMESTAMP(0),
  `created_by` int(11) NOT NULL DEFAULT -1,
  `last_updated_by` int(11) NOT NULL DEFAULT -1,
  `last_update_date` datetime(0) NOT NULL DEFAULT CURRENT_TIMESTAMP(0),
  PRIMARY KEY (`id`) USING BTREE,
  UNIQUE INDEX `plugin_driver_u1`(`plugin_id`, `plugin_version`, `tenant_id`) USING BTREE
) ENGINE = InnoDB AUTO_INCREMENT = 4 CHARACTER SET = utf8mb4 COLLATE = utf8mb4_bin ROW_FORMAT = Dynamic;

SET FOREIGN_KEY_CHECKS = 1;
