package com.github.thestyleofme.driver.mongo.session;

import java.util.Collections;
import java.util.List;
import java.util.Map;

import com.github.thestyleofme.driver.core.app.service.session.DriverSession;
import com.github.thestyleofme.driver.core.app.service.session.SessionTool;
import com.github.thestyleofme.driver.core.domain.entity.Payload;
import com.github.thestyleofme.driver.core.domain.entity.ResponseData;
import com.github.thestyleofme.driver.mongo.util.MongoTemplateUtil;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

/**
 * <p>
 * MongoDriverSession
 * </p>
 *
 * @author stone 2020/9/10 12:44
 * @since 1.0.0
 */
@Slf4j
public class MongoDriverSession implements DriverSession, SessionTool {

    private final MongoTemplateUtil mongoTemplateUtil;

    public MongoDriverSession(MongoTemplateUtil mongoTemplateUtil) {
        this.mongoTemplateUtil = mongoTemplateUtil;
    }

    @Override
    public boolean isValid() {
        return true;
    }

    @Override
    public List<String> schemaList(String... params) {
        return mongoTemplateUtil.schemas();
    }

    @Override
    public List<String> tableList(String schema) {
        return mongoTemplateUtil.tables(schema);
    }

    @Override
    public List<String> tableList(String schema, String tablePattern) {
        return this.tableList(schema);
    }

    @Override
    public ResponseData<?> get(Payload payload) {
        return mongoTemplateUtil.get(payload);
    }

    @Override
    public Long queryCount(String schema, String sql) {
        return mongoTemplateUtil.count(schema, sql);
    }

    @Override
    public void executeOneUpdate(String schema, String sql) {
        mongoTemplateUtil.update(schema, sql);
    }

    @Override
    public List<Page<Map<String, Object>>> executePageAll(String schema, String text, Pageable pageable) {
        Page<Map<String, Object>> mapPage = mongoTemplateUtil.select(schema, text, pageable);
        return Collections.singletonList(mapPage);
    }
}
