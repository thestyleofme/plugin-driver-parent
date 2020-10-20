package com.github.thestyleofme.driver.mongo.util;

import java.util.*;
import javax.servlet.http.HttpServletRequest;

import com.github.thestyleofme.driver.core.domain.entity.Payload;
import com.github.thestyleofme.driver.core.domain.entity.ResponseData;
import com.github.thestyleofme.driver.core.infra.utils.Conf;
import com.github.thestyleofme.driver.mongo.constant.Key;
import com.mongodb.*;
import com.mongodb.client.MongoDatabase;
import com.mongodb.client.MongoIterable;
import lombok.extern.slf4j.Slf4j;
import org.bson.Document;
import org.bson.types.ObjectId;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.util.StringUtils;
import org.springframework.web.bind.ServletRequestUtils;

/**
 * mongo template
 *
 * @author stone 2020/9/10 13:44
 * @since 1.0.0
 */
@Slf4j
public class MongoTemplateUtil {

    private static final String RESULT_KEY = "retval";
    private static final String BSON_EVAL = "$eval";
    private static final String SQL_COUNT = "count";

    private MongoClient mongoClient;
    private String defaultDatabase;

    public MongoTemplateUtil() {

    }

    public MongoTemplateUtil(Properties properties) {
        String host = Conf.require(properties, Key.HOST);
        String port = Conf.require(properties, Key.PORT);
        String username = Conf.require(properties, Key.USERNAME);
        String database = Conf.require(properties, Key.DEFAULT_DATABASE);
        String password = Conf.require(properties, Key.PASSWORD);

        ServerAddress address = new ServerAddress(host, Integer.parseInt(port));
        MongoCredential credential = MongoCredential.createCredential(username, database, password.toCharArray());
        MongoClientOptions options = MongoClientOptions.builder().build();
        this.mongoClient = new MongoClient(address, credential, options);
        this.defaultDatabase = database;
    }


    public List<String> schemas() {
        List<String> schemas = new LinkedList<>();
        MongoIterable<String> iterable = mongoClient.listDatabaseNames();
        for (String database : iterable) {
            schemas.add(database);
        }
        return schemas;
    }

    public List<String> tables(String schema) {
        if (StringUtils.isEmpty(schema)) {
            schema = defaultDatabase;
        }
        MongoDatabase database = mongoClient.getDatabase(schema);
        MongoIterable<String> iterable = database.listCollectionNames();
        List<String> tables = new LinkedList<>();
        for (String table : iterable) {
            tables.add(table);
        }
        return tables;
    }

    public ResponseData<?> get(Payload payload) {
        String collection = payload.getOrThrow(Key.EXPRESSION);
        HttpServletRequest request = payload.getOrThrow(Key.REQUEST);
        int page = ServletRequestUtils.getIntParameter(request, Key.PAGE, 0);
        int size = ServletRequestUtils.getIntParameter(request, Key.SIZE, 10);
        Page<Map<String, Object>> data = select(defaultDatabase, collection, PageRequest.of(page, size));
        return ResponseData.builder()
                .data(data)
                .build();
    }


    /**
     * 查询数据条数
     *
     * @param schema 数据库，可为空。为空则取当前连接的数据库
     * @param sql    如db.collection.find()
     * @return 数据条数
     */
    public Long count(String schema, String sql) {
        if (StringUtils.isEmpty(schema)) {
            schema = defaultDatabase;
        }
        MongoDatabase database = mongoClient.getDatabase(schema);
        BasicDBObject bson = new BasicDBObject();
        sql = sql + ".toArray()";
        bson.put(BSON_EVAL, sql.replace("\"", "'"));
        Document commandResult = database.runCommand(bson);
        return (long) commandResult.getList(RESULT_KEY, Document.class).size();
    }

    /**
     * 分页查询数据
     *
     * @param schema   数据库，可为空。为空则取当前连接的数据库
     * @param sql      如db.collection.find()
     * @param pageable 分页
     * @return 分页数据
     */
    public Page<Map<String, Object>> select(String schema, String sql, Pageable pageable) {
        if (StringUtils.isEmpty(schema)) {
            schema = defaultDatabase;
        }
        MongoDatabase database = mongoClient.getDatabase(schema);

        // 分页查询参数
        long offset = pageable.getOffset();
        long pageSize = pageable.getPageSize();

        log.info("offset: {}, pageSize: {}", offset, pageSize);

        //执行查询
        BasicDBObject bson = new BasicDBObject();
        String pageSql;
        if (!sql.contains(SQL_COUNT)) {
            String page = ".skip(" + offset + ").limit(" + pageSize + ")";
            pageSql = sql + page + ".toArray()";
        } else {
            pageSql = sql;
        }
        bson.put(BSON_EVAL, pageSql.replace("\"", "'"));
        // db.collection.find().skip(0).limit(10).toArray()
        Document commandResult = database.runCommand(bson);

        //分为count查询和非count查询
        List<Document> documents = new ArrayList<>();
        Document documentCount = new Document();
        long count;

        try {
            documents = commandResult.getList(RESULT_KEY, Document.class);
            count = this.count(schema, sql);
        } catch (Exception e) {
            documentCount.put(SQL_COUNT, commandResult.get(RESULT_KEY));
            count = 1;
        }

        log.info("count: {}", count);
        // 没有数据则直接返回
        if (count == 0) {
            return new PageImpl<>(Collections.emptyList(), pageable, count);
        }

        List<Map<String, Object>> resultDocuments = new LinkedList<>();

        if (!documentCount.isEmpty()) {
            resultDocuments.add(documentCount);
            return new PageImpl<>(resultDocuments, pageable, count);
        }

        for (Document document : documents) {
            // ObjectId 处理
            Object id = document.get("_id");
            if (id instanceof ObjectId) {
                ObjectId objectId = (ObjectId) id;
                document.put("_id", objectId.toHexString());
            }
            resultDocuments.add(document);
        }
        return new PageImpl<>(resultDocuments, pageable, count);
    }


    public void update(String schema, String sql) {
        if (StringUtils.isEmpty(schema)) {
            schema = defaultDatabase;
        }
        MongoDatabase database = mongoClient.getDatabase(schema);

        BasicDBObject bson = new BasicDBObject();
        sql = sql.replace("\"", "'");
        bson.put(BSON_EVAL, sql);
        try {
            database.runCommand(bson);
        } catch (Exception e) {
            log.warn("exec failed", e);
        }
    }
}
